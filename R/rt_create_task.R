#' @title Create Task folder from zip file
#' @description Create task folder from zip file, with a `.Rproj` file and try to open that in Rstudio.
#'              On Mac OS, the exercise file gets unzipped upon downloading, hence any file within the folder can be used.
#' @return exdir, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct-Dec 2020
#' @keywords file
#' @importFrom berryFunctions normalizePathCP checkFile openFile
#' @importFrom tools file_path_sans_ext
#' @importFrom utils unzip
#' @export
#' @seealso [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#'
#' @param zipfile Path to zip file (or, if unzipped, any of the files within the folder).
#'                "\" (on Windows) must be changed to "/".
#'                DEFAULT: NULL, meaning to use [file.choose()]
#' @param deletezip If the task was created sucessfully, delete the original zip file? DEFAULT: TRUE
#' @param \dots   Further arguments passed to \code{\link{unzip}}
#'
rt_create_task <- function(
zipfile=NULL,
deletezip=TRUE,
...
)
{
# Notify about closing tab:
message("If you haven't already, please close the browser tab with the CodeOcean task.",
				"\nOtherwise CodeOcean will autosave the _empty_ script there.")
rl <- readline("I have closed the browser tab (y/n, then Enter): ")
if(tolower(substr(rl,1,1)) != "y") stop("First close the browser tab.")

# File management:
if(is.null(zipfile))
	{
	message("Choose the exercise zip file. If it is unzipped, any file within the folder.",
					"\nThe interactive file choice window may be hidden...")
	Sys.sleep(0.1) # so the message gets displayed on Mac OS before file selection
  zipfile <- file.choose()
  }
zipfile <- berryFunctions::normalizePathCP(zipfile)
berryFunctions::checkFile(zipfile)

# exdir:
zipped <- grepl("\\.zip$", zipfile)
exdir <- dirname(zipfile) # remove   sth.ext   from   path/to/exercise/sth.ext
if(zipped)
  {
	exdir <- sub("\\.zip$", "", zipfile)  # remove   .zip   from   path/to/exercise.zip
  if(dir.exists(exdir)) stop("exdir already exists. Please choose a new location. exdir='", exdir, "'")
  unzip(zipfile=zipfile, exdir=exdir, ...)
  }

# create .Rproj File
rprojfile <- sub("FProg2._R_", "", basename(exdir))
rprojfile <- paste0(exdir, "/zz_",rprojfile,".Rproj")
rprojfile <- berryFunctions::normalizePathCP(rprojfile)
cat("Version: 1.0\n\nRestoreWorkspace: No\nSaveWorkspace: No\nEncoding: UTF-8", file=rprojfile)

# put tasks to Rstudio opened files list:
rt_add_opened_files(rt_read_cofile(paste0(exdir,"/.co"))$files$name, dir=exdir)

# try to open Rproject:
message("Opening ", rprojfile, "\nOpen manually if this fails.")
berryFunctions::openFile(rprojfile)

# delete zipfile:
if(deletezip && zipped) file.remove(zipfile)

# Output:
return(invisible(exdir))
}
