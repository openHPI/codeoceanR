#' @title Create exercise folder from zip file
#' @description Create exercise folder from zip file, with a `.Rproj` file and try to open that in Rstudio.
#'              On Mac Safari, the exercise file gets unzipped upon downloading, hence any file within the folder can be used.
#' @return exdir, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct-Dec 2020
#' @keywords file
#' @importFrom berryFunctions normalizePathCP checkFile openFile
#' @importFrom tools file_path_sans_ext
#' @importFrom utils unzip
#' @export
#' @seealso [rt_create_all()], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#'
#' @param zipfile Path to zip file (or, if unzipped, any of the files within the folder).
#'                "\" (on Windows) must be changed to "/".
#'                DEFAULT: NULL, meaning to use [file.choose()]
#' @param deletezip If the exercise folder was created sucessfully, delete the original zip file? DEFAULT: TRUE
#' @param ask     Ask whether browser tab has been closed? DEFAULT: TRUE
#' @param open    Open the created .Rproj file? DEFAULT: TRUE
#' @param \dots   Further arguments passed to \code{\link{unzip}}
#'
rt_create <- function(
zipfile=NULL,
deletezip=TRUE,
ask=TRUE,
open=TRUE,
...
)
{
# Notify about closing tab:
de <- rt_default_language=="de"
if(ask){
if(de)
{
message("Falls noch nicht geschehen, schlie\u00dfe bitte den Browser Tab mit der CodeOcean Aufgabe.",
				"\nSonst speichert CodeOcean periodisch das dortige leere Skript.")
rl <- readline("Ich habe den Browser Tab geschlossen (j/n, dann Enter): ")
if(!tolower(substr(rl,1,1)) %in% c("y","j")) stop("Bitte erst den Browser Tab schlie\u00DFen.")
} else
{
message("If you haven't already, please close the browser tab with the CodeOcean exercise.",
				"\nOtherwise CodeOcean will autosave the _empty_ script there.")
rl <- readline("I have closed the browser tab (y/n, then Enter): ")
if(!tolower(substr(rl,1,1)) %in% c("y","j")) stop("First close the browser tab.")
}
}
# File management:
if(is.null(zipfile))
	{
	if(de)
	message("W\u00e4hle die Zip-datei mit der Aufgabe. Falls schon entpackt, irgendeine Datei im Ordner.",
					"\nDas interaktive Datei-Auswahlfenster kann im Hintergrund sein...")
	else
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
  if(dir.exists(exdir))
  	if(de)
  	stop("exdir besteht bereits. W\u00e4hle einen neuen Ordner. exdir='", exdir, "'") else
  	stop("exdir already exists. Please choose a new location. exdir='", exdir, "'")
  unzip(zipfile=zipfile, exdir=exdir, ...)
  }

# check for .Rproj file from previous rt_create calls:
nproj <- length(dir(exdir, pattern=".Rproj"))
if(nproj>0) if(de) stop("\nEs gibt bereits eine .Rproj Datei im exdir.\n",
												"\u00D6ffne diese anstatt `rt_create` erneut zu verwenden.\nexdir='",
												exdir, "'") else stop("\nexdir already has an .Rproj file.\n",
												"Open it manually instead of running `rt_create` again.\nexdir='",
												exdir, "'")

# create .Rproj File
rprojfile <- sub("FProg2._R_", "", basename(exdir))
rprojfile <- paste0(exdir, "/zz_",rprojfile,".Rproj")
rprojfile <- berryFunctions::normalizePathCP(rprojfile)
cat("Version: 1.0\n\nRestoreWorkspace: No\nSaveWorkspace: No\nEncoding: UTF-8", file=rprojfile)

# put exercise scripts to Rstudio opened files list:
rt_add_opened_files(rt_read_cofile(paste0(exdir,"/.co"))$files$name, dir=exdir)

# try to open Rproject:
if(open){
if(de)
message("rt_create \u00f6ffnet jetzt ", rprojfile, "\n\u00d6ffne diese Datei manuell, wenn n\u00f6tig.") else
message("Opening ", rprojfile, "\nOpen manually if this fails.")
berryFunctions::openFile(rprojfile)
}

# delete zipfile:
if(deletezip && zipped) file.remove(zipfile)

# Output:
return(invisible(exdir))
}
