#' @title Get contextIdentifier for .Rproj.user ID
#' @description Internal function to find the User-specific 8 digit hexadecimal id for .Rproj.user ID
#' @return Hexadecimal ID charstring
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2020
#' @seealso [rt_add_opened_files()]
#' @keywords internal
#'
rt_get_context_id <- function()
{
# https://stackoverflow.com/a/55940249 , https://github.com/rstudio/rstudio/pull/5069
failout <- function(txt="found", fn="")
  {
	if(rt_default_language=="de")
	   {
	   txt = switch(txt, "found"="gefunden", txt)
	   warning("Die Rstudio User Settings Datei wurde nicht ",txt, if(fn!="") paste0(fn, ".\n") else ". ",
	   				"Skriptdateien in einem Rprojekt m\u00fcssen manuell ge\u00f6ffnet werden.",
	   				"\nDies sollte eigentlich nicht passieren. Bitte informiere Berry mit Angaben zum Betriebssystem um das f\u00fcr alle zu l\u00f6sen.",
	   				"\nF\u00fcr dich selbst: starte irgendein .Rproj in Rstudio und f\u00fchre dann aus:",
	   				"  codeoceanR::rt_set_context_id()  \nN\u00e4chstes mal sollte es dann klappen :).", call.=FALSE)
     } else
	warning("Rstudio User Settings file cannot be ",txt, if(fn!="") paste0(fn, ".\n") else ". ",
					"Files will not already be opened in .Rproj.",
					"\nPlease report this to Berry (with OS info) so it can be fixed in the future.",
					"\nOpen any .Rproj in Rstudio, then run:  codeoceanR::rt_set_context_id()  ",
					"to solve this for next time.", call.=FALSE)
	return("5A57A303")
  }
#
# Find Settings file
# Windows vs Mac / Linux:
if(rt_is_OS("Windows"))
{
rsdir <- Sys.getenv("LOCALAPPDATA")
rsfile <- paste0(rsdir, "/RStudio-Desktop/monitored/user-settings/user-settings")
if(!file.exists(rsfile)) rsfile <- paste0(rsdir, "/RStudio/rstudio-desktop.json") # Rstudio 1.4
if(!file.exists(rsfile)) rsfile <- "~/.rstudio_context_id_by_codeoceanR.txt" # from rt_set_context_id
if(!file.exists(rsfile)) return(failout())
} else
{
rsfile <- "~/.rstudio-desktop/monitored/user-settings/user-settings"
if(!file.exists(rsfile)) rsfile <- "~/.config/rstudio/rstudio-desktop.json" # Rstudio 1.4
if(!file.exists(rsfile)) rsfile <- "~/.config/rstudio/rstudio-state.json" # guessing from #5069...
if(!file.exists(rsfile)) rsfile <- "/etc/rstudio/rstudio-desktop.json" # guessing from https://github.com/rstudio/rstudio/issues/5301#issuecomment-533216102
if(!file.exists(rsfile)) rsfile <- "~/.local/share/rstudio/rstudio-desktop.json" # Mac OS
if(!file.exists(rsfile)) rsfile <- "~/.rstudio/rstudio-desktop.json" # just a wild guess
if(!file.exists(rsfile)) rsfile <- "~/.rstudio_context_id_by_codeoceanR.txt" # from rt_set_context_id
if(!file.exists(rsfile)) return(failout())
}
#
# Get ID from settings file:
rs <- readLines(rsfile, warn=FALSE) # rs: Rstudio Settings
                  id <- grep("contextIdentifier", rs, value=TRUE)
if(length(id)==0) id <- grep("context_id",        rs, value=TRUE) # Rstudio 1.4
if(length(id)==0) return(failout("processed for ID string: ",rsfile))
id <- trimws(id)
id <- sub("contextIdentifier=\"", "", id)
id <- sub("\"context_id\": \"", "", id)
id <- gsub("\"", "", id)
id <- gsub(",", "", id)
if(nchar(id)==0) return(failout("splitted for ID string: ", rsfile))
return(id)
}



#' @title Try to set contextIdentifier from .Rproj.user ID
#' @description Try to find the User-specific 8 digit hexadecimal id from .Rproj.user ID.
#'              Then save it to a file so that it can later be found by [rt_get_context_id()].
#'              If successful, it will be saved in "~/.rstudio_context_id_by_codeoceanR.txt".
#' @return Hexadecimal ID charstring
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2020
#' @seealso [rt_get_context_id()]
#' @importFrom berryFunctions checkFile normalizePathCP
#' @export
#' @param dir Path to some folder containing a .Rproj file
#'            and hence a (hidden) ".Rproj.user" folder. DEFAULT: "."
#'
rt_set_context_id <- function(dir=".")
{
dir <- berryFunctions::normalizePathCP(dir)
berryFunctions::checkFile(dir)
ids <- dir(paste0(dir,"/.Rproj.user"), pattern="^[a-zA-Z0-9]{8}$")
ids <- ids[ids!="5A57A303"] # exclude failure output from rt_get_context_id
de <- rt_default_language=="de"
if(length(ids)==0)
	if(de) stop("Finde keine context ID im Ordner ", dir) else
		     stop("Could not find any context ID in dir ", dir)
if(length(ids)!=1)
	if(de) warning("Finde ", length(ids), " context IDs im Ordner ", dir, "\nFahre fort mit ", ids[1]) else
	       warning("Found ", length(ids), " context IDs in dir ", dir, "\nProceeding with ", ids[1])
ids <- ids[1]
cat(paste0('contextIdentifier="',ids,'"'), file="~/.rstudio_context_id_by_codeoceanR.txt")
return(ids)
}
