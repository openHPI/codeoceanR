#' @title Select section of a script
#' @description Select the section of the script code between `t*_start` and `t*_end` for further analysis in tests.
#' For manual evaluation, use [`eval`]`(`[`str2expression`]`(code))`,
#' not [`str2lang`], in case students use line breaks.
#' @return FALSE for failure, otherwise selected lines of the script,
#'         by default NOT collapsed (linebreaks replaced with ;) through [paste]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020 + Nov 2021
#' @seealso [rt_test_task], [rt_run_script], [rt_has_args],
#'          [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords file
#'
#' @param script      Charstring with several elements, normally output from [rt_run_script].
#' @param snumber     Section to be found, eg for snumber=3, the lines between `t3_start` and `t3_end`.
#' @param name        [rt_warn] name. DEFAULT: deparse(substitute(script))
#' @param collapse    Replacement for linebreaks. For nice inclusion in messages or
#'                    custom grepl tests without if(grepl(...)) error the condition has length > 1.
#'                    DEFAULT: NULL (not collapsed)
#' @param maxlen      Maximum allowed length of lines of code. DEFAULT: 95
#'
rt_script_section <- function(
script,
snumber,
name=deparse(substitute(script)),
collapse=NULL,
maxlen=95
){
  force(name)
  if(isFALSE(script)) return(FALSE) # non-existent files
  # Find task markers, warn if this fails:
  m1 <- paste0("t",snumber,"_start") # marker
  m2 <- paste0("t",snumber,"_end")
  l1 <- grep(m1, script)        # line number
  l2 <- grep(m2, script)
  ll1 <- length(l1)
  ll2 <- length(l2)
  if(ll1!=1) return(rt_warn("Found ",ll1," instances of '# ",m1,"' in ",name,"."))
  if(ll2!=1) return(rt_warn("Found ",ll2," instances of '# ",m2,"' in ",name,"."))
  if(l2 < l1)return(rt_warn("'# ",m2,"' must come after '# ",m1,"' in ",name,"."))
  #
  # Process script between markers:
  sl <- trimws(script[(l1+1):(l2-1)])
  sl <- sl[sl!=""]
  sl <- sl[!grepl("^#", sl)]
  if(length(sl)<1) return(rt_warn("The code section t",snumber," is empty."))
  long <- nchar(sl)>maxlen
  if(any(long))	return(rt_warn("Use line breaks in code section t",snumber,
  	": max ",maxlen," symbols per line, not ", paste0(nchar(sl)[long],collapse=","),"."))
  if(!is.null(collapse)) sl <- paste(sl, collapse=collapse)
  # remove duplicate ";" if someone has ; at the end of a line in their script already
  sl <- gsub(";;", ";", sl, fixed=TRUE)
  sl
}
