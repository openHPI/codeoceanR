#' @title Read .co file
#' @description Read .co file (from downloaded CO task).
#' @return List with elements 'token', 'url', 'files'
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2021
#' @seealso \code{\link{rt_score}}
#' @keywords file
#' @importFrom berryFunctions normalizePathCP l2df
#' @param cofile Path of .co file, may be relative.
#'
rt_read_cofile <- function(cofile)
{
# check if dir is an exercise directory
dir <- berryFunctions::normalizePathCP(dirname(cofile))
if(!file.exists(cofile))
  if(rt_default_language=="de")
  stop("Dies ist kein Verzeichnis mit einer CodeOcean Aufgabe: ", dir,
    "\nDie '.co' Datei existiert nicht.",
    "\nHast du rt_unzip_exes() ausgef\u00fchrt? Wurde die '00_*.Rproj' Datei ge\u00f6ffnet?",
    "\nRechtsoben in Rstudio sollte das R-Projekt-Symbol den Kursordner zeigen.", call.=FALSE) else
  stop("This is not a CodeOcean exercise directory: ", dir,
    "\nThe file '.co' does not exist.",
    "\nDid you run rt_unzip_exes()? Did you open the '00_*.Rproj' file?",
    "\nThe Rstudio top right R Project symbol should show your course folder name.", call.=FALSE)

co <- readLines(cofile, warn=FALSE)
out <- list()
out$token <- co[1]
out$url   <- co[2]

co_files <- co[-(1:2)]
if(length(co_files)<1)
  if(rt_default_language=="de")
    stop("In der Datei '",cofile,"' sind keine Dateinamen zum Hochladen gelisted.") else
    stop("No filenames found to be submitted in file '",cofile,"'")
co_files <- berryFunctions::l2df(strsplit(co_files, "="))
colnames(co_files) <- c("name", "id")
out$files <- co_files

return(out)
}
