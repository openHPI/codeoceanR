#' @title run student-submitted scripts
#' @description Run script, with informative [rt_warn] message if [source()] fails.
#' @return FALSE for failure, otherwise contents of the script, output of [readLines()]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_script_runs], [rt_select_script_section],
#'          [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords file
#' @export
#'
#' @param filename Path of script to be run
#'
rt_run_script <- function(filename){
  rt_env(id=paste0(" ", filename))
  if(!file.exists(filename)) {rt_warn("This file does not exist: '", filename,
                                      "'. current getwd: ", getwd()); return(FALSE)}
  #           # exclude recursive score calls:
              fcontent <- readLines(filename, warn=FALSE)
              excl <- grepl("rt_local_score(", fcontent, fixed=TRUE) |
                      grepl("rt_score("      , fcontent, fixed=TRUE)
              fnew <- fcontent[!excl]
              writeLines(fnew, filename)
  e <- try(source(filename), silent=TRUE)
              writeLines(fcontent, filename)
  if(inherits(e, "try-error")) {
    rt_warn("can not be executed. Make sure each line can be run.",
            if(!interactive()) "\nFor CO in browser: Click 'RUN' to view the error and then fix it.",
            "\n--- The source() error message was: ", e)
    return(FALSE)}
  readLines(filename, warn=FALSE)
}
