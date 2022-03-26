#' @title Check for files with unsaved changes
#' @description Raise an informative error/warning message if any of the opened
#'              Rstudio R project file tabs contain unsaved changes.
#' @return NULL if all files are saved. Otherwise, if `warnonly=TRUE`, the names of changed but unsaved files.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @importFrom berryFunctions checkFile normalizePathCP
#' @seealso [rt_score]
#'
#' @param dir      Path to (processed) exercise folder.
#'                 Must contain ".Rproj.user" folder. DEFAULT: "."
#' @param warnonly Logical: Only issue a [warning] instead of an
#'                 error with [stop]? DEFAULT: FALSE
#'
rt_check_for_unsaved_files <- function(dir=".", warnonly=FALSE)
{
de <- rt_default_language=="de"
# check dir:
dir <- berryFunctions::normalizePathCP(paste0(dir, "/.Rproj.user"))
if(warnonly && !dir.exists(dir)) return(if(de)
	message("rt_score pr\u00FCft nicht auf ungespeicherte Datei\u00E4nderungen (dies ist kein Rstudio Rprojekt).") else
	message("Not checking for unsaved file changes (this is not an Rstudio Rproject)."))
berryFunctions::checkFile(dir, warnonly=warnonly)
# find folder:
dir <- dir(dir, pattern="^[a-zA-Z0-9]{6,8}$", full.names=TRUE)
dir <- dir[!grepl("shared$", dir)] # safety check...
dir <- paste0(dir, "/sources")
berryFunctions::checkFile(dir, warnonly=warnonly)
# temporary id folder:
dir <- dir(dir, pattern="^s-[a-zA-Z0-9]{8}$", full.names=TRUE)
fn  <- dir(dir, pattern=  "^[a-zA-Z0-9]{8}$", full.names=TRUE)
# Finish if no files are open:
if(length(fn)==0) return(NULL)
# Get names of files with unsaved changes:
ct <- lapply(fn, readLines, warn=FALSE)
fn <- lapply(ct, function(x)
  {
  unsavedchanges <- any(grepl('dirty": true', x))
  if(!unsavedchanges) return(character())
  out <- grep('"path":', x, value=TRUE)
  out <- strsplit(out, ': "')[[1]][2]
  out <- strsplit(out, '"')[[1]][1]
  out
  })
fn <- unlist(fn)
# Prepare message:
if(  length(fn)<1  ) return(NULL)
p <- length(fn)>1 # plural?
if(de)
msg <- paste0("Die folgende Datei", if(p)"en haben" else " hat", " ungespeicherte \u00c4nderungen:\n- ",
		paste(fn, collapse="\n- "), "\nSpeichere diese (CTRL+S) und f\u00fchre rt_score() erneut aus.")
else
msg <- paste0("The following file", if(p)"s", " contain", if(!p)"s", " unsaved changes:\n- ",
		 paste(fn, collapse="\n- "), "\nSave (CTRL+S) and run rt_score() again.")
if(warnonly) warning(msg, call.=FALSE) else stop(msg, call.=FALSE)
return(fn)
}
