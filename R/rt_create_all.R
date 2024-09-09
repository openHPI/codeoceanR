#' @title Create exercise folders from several zip files
#' @description Create exercise folder from all existing zip files.
#'              Unlike [rt_create()], this is not compatible with Mac Safari autounzipping.
#'              Also, the exercises are not opened in Rstudio.
#' @return dirnames, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2023
#' @keywords file
#' @importFrom berryFunctions normalizePathCP checkFile openFile
#' @importFrom rstudioapi selectDirectory
#' @export
#' @seealso [rt_create()]
#'
#' @param path Folder containing (several) downloaded zip files.
#'                "\" (on Windows) must be changed to "/".
#'                DEFAULT: NULL, meaning to use [rstudioapi::selectDirectory()]
#' @param \dots   Further arguments passed to \code{\link{rt_create}}
#'
rt_create_all <- function(
path=NULL,
...
)
{
# Notify about closing tab:
de <- rt_default_language=="de"
if(de)
{
message("Falls noch nicht geschehen, schlie\u00dfe bitte alle Browser Tabs mit den CodeOcean Aufgaben.",
				"\nSonst speichert CodeOcean periodisch die dortigen leeren Skripte.")
rl <- readline("Ich habe die Browser Tabs geschlossen (j/n, dann Enter): ")
if(!tolower(substr(rl,1,1)) %in% c("y","j")) stop("Bitte erst die Browser Tabs schlie\u00DFen.")
} else
{
message("If you haven't already, please close all browser tabs with CodeOcean exercises.",
				"\nOtherwise CodeOcean will autosave the _empty_ scripts there.")
rl <- readline("I have closed the browser tabs (y/n, then Enter): ")
if(!tolower(substr(rl,1,1)) %in% c("y","j")) stop("First close the browser tabs.")
}
# Path management:
if(is.null(path))
 {
 msg <- if(de) "Waehle den Ordner mit den R Uebung Zips" else "Choose the folder containing R zip exercises"
 message(msg)
 Sys.sleep(0.1) # so the message gets displayed on Mac OS before file selection
 path <- rstudioapi::selectDirectory(msg)
 }
# create exes:
exes <- dir(path, pattern="\\.zip$", full.names=TRUE)
if(length(exes)<1) if(de) stop("Keine zip Dateien gefunden in ", path) else stop("No zip files found at ", path)
exdirs <- sapply(exes, rt_create, ask=FALSE, open=FALSE, ..., USE.NAMES=FALSE)
exprojs <- dir(exdirs, pattern="\\.Rproj$", full.names=TRUE)
exprojs <- gsub(paste0(path,"/"), "", exprojs)
msg <- if(de) "\nErledigt!\n\u00D6ffne manuell folgende Dateien unter " else "\nDone!\nManually open the following files at "
message(msg, path, ":\n- ", paste(exprojs, collapse="\n- "))
# Output:
return(invisible(exdirs))
}
