#' @title Create Task folder from zip file
#' @description Create task folder from zip file, with a `.Rproj` file and try to open that in Rstudio.
#' @return exdir, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords file
#' @importFrom berryFunctions normalizePathCP checkFile openFile
#' @importFrom tools file_path_sans_ext
#' @importFrom utils unzip
#' @export
#' @seealso [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#'
#' @param zipfile Path to zip file. Remember: on Windows, "\" must be changed to "/".
#'                On Mac OS, the file gets unzipped upon downloading,
#'                so it must be any one of the files within that folder.
#'                DEFAULT: NULL, meaning to use [file.choose()]
#' @param exdir   Folder to unzip to, e.g. "./task" for folder at current wd.
#'                `exdir` may not yet exist, to avoid overwriting previously
#'                unzipped and potentially edited tasks.
#'                `exdir` must remain NULL if `isunzipped=TRUE` (on Mac OS).
#'                DEFAULT: NULL (path from zipfile).
#' @param isunzipped Is the quiz already unzipped to a folder? Then that folder will be used.
#'                DEFAULT: [rt_is_OS]`("Mac")`
#' @param deletezip If the task was created sucessfully, delete the original zip file? DEFAULT: TRUE
#' @param \dots   Further arguments passed to \code{\link{unzip}}
#'
rt_create_task <- function(
zipfile=NULL,
exdir=NULL,
isunzipped=rt_is_OS("Mac"),
deletezip=TRUE,
...
)
{
# Notify about closing tab:
message("If you haven't already, please close the browser tab with the CodeOcean task.",
				"\nOtherwise CodeOcean will autosave the _empty_ script there.")
rl <- readline("I have closed the browser tab (y/n, then Enter): ")
if(tolower(substr(rl,1,1)) != "y") stop("First close the browser tab.")
# File name management:
if(is.null(zipfile))
	{
	message(if(isunzipped) "Choose any file within the quiz folder." else
	                       "Choose the quiz zip file.",
												 " The interactive file choice window may be hidden...")
  zipfile <- file.choose()
  }
zipfile <- berryFunctions::normalizePathCP(zipfile)
berryFunctions::checkFile(zipfile)
if(isunzipped)
{
if(!is.null(exdir)) warning("isunzipped=TRUE, yet exdir is given. It will be ignored.")
exdir <- dirname(zipfile)
zipfile <- exdir # to get the folder name in rprojfile
} else
{
if(tools::file_ext(zipfile)!="zip") stop("The input must be a zip file. It was: ", zipfile)
if(is.null(exdir)) exdir <- tools::file_path_sans_ext(zipfile)
if(dir.exists(exdir)) stop("exdir already exists. Please choose a new location. exdir=", exdir)
# unzip:
unzip(zipfile=zipfile, exdir=exdir, ...)
}
# create .Rproj File
rprojfile <- paste0(exdir, "/zz_",tools::file_path_sans_ext(basename(zipfile)),".Rproj")
rprojfile <- berryFunctions::normalizePathCP(rprojfile)
cat("Version: 1.0\n\nRestoreWorkspace: No\nSaveWorkspace: No\nEncoding: UTF-8", file=rprojfile)
# put tasks to Rstudio opened files list:
rt_add_opened_files(dir(exdir,pattern="script_"), dir=exdir)
# try to open Rproject:
message("Opening ", rprojfile, "\nOpen manually if this fails.")
berryFunctions::openFile(rprojfile)
# delete zipfile
if(deletezip & !isunzipped) file.remove(zipfile)
# Output:
return(invisible(exdir))
}
