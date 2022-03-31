#' @title run student-submitted scripts
#' @description Run script, with informative [rt_warn] message if [source()] fails.
#' @return FALSE for failure, otherwise contents of the script, output of [readLines()]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_test_task], [rt_script_section],
#'          [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords file
#' @export
#'
#' @param filename Path of script to be run
#' @param quiet    If TRUE, suppress printed output, messages and warnings.
#'                 Errors are handled separately. DEFAULT: TRUE
#' @param echo     If TRUE, return the [sink()] log of [source()] with `echo=TRUE`
#'                 instead of the raw script lines.
#'
rt_run_script <- function(filename, quiet=TRUE, echo=FALSE){
  rt_env(id=paste0(" ", filename))
  if(!file.exists(filename)) {
  	rt_env(info=paste0("script failed: ",filename))
  	rt_warn(en="This file does not exist: '",de="Diese Datei existiert nicht: ", filename,
  					en="'. current getwd: ", de="'. Aktuelles getwd Verzeichnis: ", getwd())
  	return(FALSE)
  	}
  # exclude recursive score calls:
  fcontent <- readLines(filename, warn=FALSE, encoding="UTF-8")
  excl <- grepl("rt_local_score(", fcontent, fixed=TRUE) |
          grepl("rt_score("      , fcontent, fixed=TRUE)
  fnew <- fcontent[!excl]
  tfile <- tempfile(fileext="_coscript.R")
  lfile <- tempfile(fileext="_colog.txt")
  writeLines(fnew, tfile)
  # actually source the (modified) file:
  sink(lfile)
  if(!quiet)
  	e <- try(source(tfile, local=parent.frame(), echo=echo), silent=TRUE) else
    e <- try(suppressWarnings(suppressMessages(source(tfile, local=parent.frame(), echo=echo))), silent=TRUE)
  sink()
  if(inherits(e, "try-error")) {
  	rt_env(info=paste0("script failed: ",filename))
  	msg <- if(rt_env()$lang=="de") "Fehler in Zeile:Spalte " else "Error in line:column "
    e <- sub("^Error in source.*_coscript.R:",msg,e)
    e <- gsub("\n"," ",e)
    e <- gsub("\\s+", " ", e)
    rt_warn(en="can not be executed. Make sure each line can be run.",
    				de="kann nicht ausgef\u00FChrt werden. Sorge, dass jede Zeile fehlerfrei l\u00E4uft.",
            "\n--- source() ", en="message: ", de="Meldung: ", e)
    }

  # View Error in CO informative message
  nocom <- try(as.character(parse(text=fnew)), silent=TRUE)
  if(any(grepl("View(", nocom, fixed=TRUE))) rt_warn("Kommentiere den View Aufruf aus. View f\u00FChrt auf CodeOcean zu einem Fehler.")

  if(echo) return(readLines(lfile)) else return(fcontent)
}
