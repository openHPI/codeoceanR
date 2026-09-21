#' @title Create exercise folders from zip files
#' @description Create exercise folder from each existing zip file.
#' @return exdir, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sept 2026
#' @keywords file
#' @importFrom berryFunctions normalizePathCP checkFile newFilename
#' @importFrom tools file_path_sans_ext
#' @importFrom utils unzip
#' @importFrom rstudioapi selectDirectory
#' @export
#' @seealso [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#'
#' @param path Exercise folder containing (several) downloaded zip files.
#'             Use `path=NULL` to have it use [rstudioapi::selectDirectory()].
#'             DEFAULT: "." (current working directory)
#'
unzip_exes <- function(
path="."
)
{
de <- rt_default_language=="de"
# Path selection:
if(is.null(path))
 {
 msg <- if(de) "Waehle den Ordner mit den Uebung Zips" else "Choose the folder containing zipped exercises"
 message(msg)
 Sys.sleep(0.1) # so the message gets displayed on Mac OS before file selection
 path <- rstudioapi::selectDirectory(msg)
 }
# check path
path <- berryFunctions::normalizePathCP(path)
berryFunctions::checkFile(path)
# find zipped exes:
exes <- dir(path, pattern="^FP_.*\\.zip$", full.names=TRUE)
if(length(exes)<1) if(de) stop("Keine FP zip Dateien gefunden in ", path) else stop("No FP zip files found at ", path)
exes <- berryFunctions::normalizePathCP(exes)
# function to unzip, then delete original zipfile and unnecessary files
rt_unzip <- function(zf)
  {
  folder1 <- tools::file_path_sans_ext(zf)
  folder2 <- berryFunctions::newFilename(folder1, quiet=TRUE) # avoid overwriting existing folder
  if(folder1!=folder2) message("Folder already existed: ", basename(folder1))
  unzip(zf, exdir=folder2)
  file.remove(zf)
  unlink(file.path(folder2, c("Exercise.txt", ".scripts")), recursive=TRUE)
  return(folder2)
  }
exdirs <- sapply(exes, rt_unzip, USE.NAMES=FALSE)
msg1 <- if(de) "Erledigt!\n\u00D6ffne manuell Dateien unter " else "Done!\nManually open files at "
msg2 <- if(de) paste0("\nFalls noch nicht geschehen, schlie\u00dfe bitte alle ",
											"Browser Tabs mit den CodeOcean Aufgaben.\n",
											"Sonst speichert CodeOcean periodisch die dortigen leeren Skripte.")
     else paste0("\nIf you haven't already, please close all browser tabs with CodeOcean exercises.\n",
     						"Otherwise CodeOcean will autosave the _empty_ scripts there.")
message(msg1, path, " in:\n- ", paste(basename(exdirs), collapse="\n- "), msg2)
# Output:
return(invisible(exdirs))
}
