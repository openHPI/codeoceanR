#' @title Add files to list of Rstudio opened source documents
#' @description Add files to the list of Rstudio opened source documents.
#' The related .Rproj Rstudio project should be closed at the time of writing.
#' @return expanded filename, invisibly.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_create_task()], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords internal
#'
#' @param file Filename to be added.
#' @param dir  Target directory with a .Rproj file. DEFAULT: "." [getwd()]
#' @param contextid Hexadecimal user ID, see [rt_get_context_id()]
#'
rt_file2openedlist <- function(file, dir=".", contextid=rt_get_context_id())
{
# check file and dir:
fullfile <- paste0(dir,"/",file)
berryFunctions::checkFile(fullfile, pwd=FALSE)
berryFunctions::checkFile(dir)
dir <- berryFunctions::normalizePathCP(dir)
# check whether .Rproj file is present:
if(!any(grepl(".Rproj$", dir(dir)))) stop("No .Rproj file found at dir ", dir)
# create hidden directory for list of Rstudio opened source documents:
sfdir <- paste0(dir, "/.Rproj.user/",contextid,"/sources/per/t/")
if(!dir.exists(sfdir)) dir.create(sfdir, recursive=TRUE)
# generate (increasing) project-unique ID:
id <- rt_nextlargestid(  dir(sfdir)[!grepl("contents",dir(sfdir))]   ) # function below
# determine type:
type <- switch(tools::file_ext(file),
	R="r_source",
	txt="text",
	md="markdown")
# Write file metadata:
cat('{
    "id": "',id,'",
    "path": "',fullfile,'",
    "type": "',type,'",
    "project_path": "',file,'",
    "created": 0,
    "dirty": false,
    "source_on_save": true
}
', file=paste0(sfdir, id), sep="")
# copy file content:
file.copy(from=fullfile, to=paste0(sfdir, id, "-contents"))
# set active tab to first:
panedir <- paste0(dir, "/.Rproj.user/",contextid,"/pcs/")
if(!dir.exists(panedir)) dir.create(panedir)
cat('{
    "activeTab": 0
}', file=paste0(panedir, "source-pane.pper"), sep="")
# output
return(invisible(fullfile))
}



#' @title Get contextIdentifier for .Rproj.user ID
#' @description Internal function to find the User-specific 8 digit hexadecimal id for .Rproj.user ID
#' @return Hexadecimal ID charstring
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2020
#' @seealso [rt_file2openedlist()]
#' @keywords internal
#'
rt_get_context_id <- function()
{
# https://stackoverflow.com/a/55940249 , https://github.com/rstudio/rstudio/pull/5069
failout <- function(txt="found")
  {
	warning("Rstudio User Settings file cannot be ",txt, ". Files will not already be opened in .Rproj.", call.=FALSE)
	return("5A57A303")
  }
#
# Find Settings file
# Windows vs Mac / Linux:
if(Sys.info()["sysname"]=="Windows")
{
rsdir <- Sys.getenv("LOCALAPPDATA")
rsfile <- paste0(rsdir, "/RStudio-Desktop/monitored/user-settings/user-settings")
if(!file.exists(rsfile)) rsfile <- paste0(rsdir, "/RStudio/rstudio-desktop.json") # Rstudio 1.4
if(!file.exists(rsfile)) return(failout())
} else
{
rsfile <- "~/.rstudio-desktop/monitored/user-settings/user-settings"
if(!file.exists(rsfile)) rsfile <- "~/.config/rstudio/rstudio-desktop.json" # Rstudio 1.4
if(!file.exists(rsfile)) rsfile <- "~/.rstudio/rstudio-desktop.json" # just a wild guess
if(!file.exists(rsfile)) return(failout())
}
#
# Get ID from settings file:
rs <- readLines(rsfile, warn=FALSE) # rs: Rstudio Settings
                  id <- grep("contextIdentifier", rs, value=TRUE)
if(length(id)==0) id <- grep("context_id",        rs, value=TRUE) # Rstudio 1.4
if(length(id)==0) return(failout("processed for ID string"))
id <- berryFunctions::removeSpace(id)
id <- sub("contextIdentifier=\"", "", id)
id <- sub("\"context_id\": \"", "", id)
id <- gsub("\"", "", id)
id <- gsub(",", "", id)
if(nchar(id)==0) return(failout("splitted for ID string"))
return(id)
}



#' @title Find next largest 8 digit hexadecimal id
#' @description Internal function to find the next largest 8 digit hexadecimal id
#' @return Hexadecimal ID charstring
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_file2openedlist()], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords internal
#' @examples
#' \dontrun{ # non-exported function cannot have autorun examples
#' # random hex name:
#' id <- paste0(sample(c(0:9,LETTERS[1:6]), 8, TRUE), collapse="")
#' id; rt_nextlargestid(id)
#' rt_nextlargestid("ABCDEFAB")
#' rt_nextlargestid("ABCD9999")
#' rt_nextlargestid("ABCD6789")
#' rt_nextlargestid("12345678")
#' rt_nextlargestid("A0000000")
#' }
#'
#' @param id Character string, see examples
#'
rt_nextlargestid <- function(id)
{
	if(length(id)==0) return("A0000000")
	idm <- max(id)
	if(!grepl("[0-8]", idm)) return("A0000000")
	addOne <- function(id)
	  {
		id <- strsplit(id,"")[[1]]
		wm <- max(which(id < 9))
		id[wm] <- as.numeric(id[wm]) + 1
		paste(id, collapse="")
	  }
	id <- idm
	while(id<=idm)  id <- addOne(id)
	return(id)
}

