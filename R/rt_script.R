# in 2019, CO had an older R version without isFALSE, hence manual declaration:
if(!exists("isFALSE")) isFALSE <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) && !x


#' @title run student-submitted scripts
#' @description Run script, with informative [rt_warn] message if [source()] fails.
#' @return FALSE for failure, otherwise contents of the script, output of [readLines()]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_script_runs], [rt_select_script_section], [source]
#' @keywords file
#' @export
#' @examples
#' # ToDo: link to file
#' # ToDo: consider adding message from try() to rt_warn
#'
#' @param filename Path of script to be run
#'
rt_run_script <- function(filename){
  if(!file.exists(filename)) {rt_warn("This file does not exist: '", filename,
                                      "'. current getwd: ", getwd()); return(FALSE)}
  e <- try(source(filename), silent=TRUE)
  if(inherits(e, "try-error")) {
    rt_warn("'", filename, "' can not be executed. ",
            if(!interactive()) "Click 'RUN' to view the error and then fix it." else "Make sure each line can be run.")
    return(FALSE)}
  readLines(filename, warn=FALSE)
}


#' @title test whether script can be run
#' @description test whether script can be run
#' @return Logical. No [rt_warn] message issued, that already happens in [rt_run_script]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_run_script]
#' @export
#' @examples
#' # ToDo: link to file
#'
#' @param scriptobject Charstring output from [rt_run_script] or [rt_select_script_section].
#'                     Will be checked with \code{!\link{isFALSE}()}.
#'
rt_script_runs <- function(scriptobject){
  !isFALSE(scriptobject)
}


#' @title Select section of a script
#' @description Select the section of the script code between `t*_start` and `t*_end` for further analysis in tests.
#'              The code is not executed, as this is already done in [rt_run_script].
#' @return FALSE for failure, otherwise selected lines of the script,
#'         by default collapsed (linebreaks replaced with ;) through [paste]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_run_script], [rt_script_runs], [rt_has_argument]
#' @keywords file
#' @importFrom berryFunctions removeSpace
#' @export
#' @examples
#' # ToDo: link to file
#'
#' @param scriptlines Charstring with several elements, normally output from [rt_run_script]
#' @param task_nr     Task number to be found, eg for task_nr=3, the lines between `t3_start` and `t3_end`.
#' @param collapse    Replace linebreaks with `;` (e.g. for nice inclusion in messages)? DEFAULT: TRUE
#'
rt_select_script_section <- function(scriptlines, task_nr, collapse=TRUE){
  if(isFALSE(scriptlines)) return(FALSE)
  # Find task markers, warn if this fails:
  m1 <- paste0("t",task_nr,"_start") # marker
  m2 <- paste0("t",task_nr,"_end")
  l1 <- grep(m1, scriptlines)        # line number
  l2 <- grep(m2, scriptlines)
  ll1 <- length(l1)
  ll2 <- length(l2)
  if(ll1!=1) {rt_warn("Found ",ll1," instances of '# ",m1,"' in your script."); return(FALSE)}
  if(ll2!=1) {rt_warn("Found ",ll2," instances of '# ",m2,"' in your script."); return(FALSE)}
  if(l2 < l1){rt_warn("'# ",m2,"' must come after '# ",m1,"' in your script."); return(FALSE)}
  #
  # Process script between markers:
  sl <- berryFunctions::removeSpace(scriptlines[(l1+1):(l2-1)])
  sl <- sl[sl!=""]
  sl <- sl[!grepl("^#", sl)]
  if(length(sl)<1) {rt_warn("The code section t",task_nr," is empty."); return(FALSE)}
  if(collapse) sl <- paste(sl, collapse=";")
  sl
}

