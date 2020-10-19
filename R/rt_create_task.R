#' @title Create Task folder from zip file
#' @description Create task folder from zip file, with a `.Rproj` file and try to open that in Rstudio.
#' @return exdir, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords file
#' @importFrom berryFunctions normalizePathCP checkFile openFile
#' @importFrom tools file_path_sans_ext
#' @importFrom utils unzip
#' @export
#' @examples
#' # rt_create_task("C:/Dropbox/R/FREELANCING/HPI_2020_R/CodeOceanTesting/FProg20_R_quiz_1.zip")
#'
#' @param zipfile Path to zip file. Remember: on Windows, "\" must be changed to "/".
#' @param exdir   Folder to unzip to, e.g. "." for current wd.
#'                DEFAULT: NULL (path from zipfile).
#' @param \dots   Further arguments passed to \code{\link{unzip}}
#'
rt_create_task <- function(
zipfile,
exdir=NULL,
...
)
{
# File name management:
zipfile <- berryFunctions::normalizePathCP(zipfile)
berryFunctions::checkFile(zipfile)
if(is.null(exdir)) exdir <- tools::file_path_sans_ext(zipfile)
# unzip:
unzip(zipfile=zipfile, exdir=exdir, ...)
# create .Rproj File
rprojfile <- paste0(exdir, "/zz_",tools::file_path_sans_ext(basename(zipfile)),".Rproj")
cat("Version: 1.0\n\nRestoreWorkspace: No\nSaveWorkspace: No\nEncoding: UTF-8", file=rprojfile)
# put tasks to Rstudio opened files list:
lapply(dir(exdir, pattern="script_"), rt_file2openedlist, dir=exdir)
# try to open Rproject:
message("Opening ", rprojfile, "\nOpen manually if this fails.")
berryFunctions::openFile(rprojfile)
# Output:
return(invisible(exdir))
}
