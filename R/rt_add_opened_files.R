#' @title Add files to list of Rstudio opened source documents
#' @description Add files to the list of Rstudio opened source documents.
#' The related .Rproj Rstudio project should be closed at the time of writing.
#' @return expanded filenames, invisibly.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_create()], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords internal
#'
#' @param file Filename(s) to be added, without path, must be present at `dir`.
#' @param dir  Target directory with a .Rproj file. DEFAULT: "." [getwd()]
#' @param contextid Hexadecimal user ID, see [rt_get_context_id()]
#'
rt_add_opened_files <- function(files, dir=".", contextid=rt_get_context_id())
{
# check files and dir ----
if(rt_is_OS("Linux")) files <- rev(files) # Not sure if this is needed on all Linux versions...
#
fullfiles <- paste0(dir,"/",files)
names(fullfiles) <- files
berryFunctions::checkFile(fullfiles, pwd=FALSE)
berryFunctions::checkFile(dir)
dir <- berryFunctions::normalizePathCP(dir)
# check whether .Rproj file is present:
if(!any(grepl(".Rproj$", dir(dir)))) stop("No .Rproj file found at dir ", dir)
# create hidden directory for list of Rstudio opened source documents:
sfdir <- paste0(dir, "/.Rproj.user/",contextid,"/sources/per/t/")
if(!dir.exists(sfdir)) dir.create(sfdir, recursive=TRUE)

# actually add files ----
for(file in files)
{
# generate (increasing) project-unique ID:
id <- rt_nextlargestid(  dir(sfdir)[!grepl("contents",dir(sfdir))]   )
# determine type:
type <- switch(tools::file_ext(file),
	R="r_source",
	txt="text",
	md="markdown")
# Write file metadata:
cat('{
    "id": "',id,'",
    "path": "',fullfiles[file],'",
    "type": "',type,'",
    "project_path": "',file,'",
    "created": 0,
    "dirty": false,
    "relative_order": ',rt_nextlargestorder(sfdir),',
    "properties": {
        "docOutlineVisible": "1"
    },
    "source_on_save": false
}
', file=paste0(sfdir, id), sep="")
# copy file content:
file.copy(from=fullfiles[file], to=paste0(sfdir, id, "-contents"))
}

# set active tab to first ----
panedir <- paste0(dir, "/.Rproj.user/",contextid,"/pcs/")
if(!dir.exists(panedir)) dir.create(panedir)
cat('{
    "activeTab": 0
}', file=paste0(panedir, "source-pane.pper"), sep="")

# output
return(invisible(fullfiles))
}
