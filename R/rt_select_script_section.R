#' @title Select section of a script
#' @description Select the section of the script code between `t*_start` and `t*_end` for further analysis in tests.
#'              The code is not executed, as this is already done in [rt_run_script].\\
#'              Attenton: if the user script contains line breaks and is read with
#'              `collapse=TRUE`, [rt_has_argument] leads to a test script failure!
#' @return FALSE for failure, otherwise selected lines of the script,
#'         by default NOT collapsed (linebreaks replaced with ;) through [paste]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_test_task], [rt_run_script], [rt_has_argument],
#'          [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords file
#' @export
#'
#' @param scriptlines Charstring with several elements, normally output from [rt_run_script]
#' @param task_nr     Task number to be found, eg for task_nr=3, the lines between `t3_start` and `t3_end`.
#' @param collapse    Replace linebreaks with `;` (e.g. for nice inclusion in messages)?
#'                    Also helpful if you want do run your own grepl tests on it
#'                    to not have a vector and if(grepl(...)) error the condition has length > 1.
#'                    DEFAULT: FALSE
#'
rt_select_script_section <- function(scriptlines, task_nr, collapse=FALSE){
  rt_env(id=task_nr)
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
  sl <- trimws(scriptlines[(l1+1):(l2-1)])
  sl <- sl[sl!=""]
  sl <- sl[!grepl("^#", sl)]
  if(length(sl)<1) {rt_warn("The code section t",task_nr," is empty."); return(FALSE)}
  if(collapse) sl <- paste(sl, collapse=";")
  # remove duplicate ";" if someone has ; at the end of a line in their script already
  sl <- gsub(";;", ";", sl, fixed=TRUE)
  sl
}
