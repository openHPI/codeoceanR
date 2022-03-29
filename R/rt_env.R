#' @title interact with rt_test_env
#' @description interact with `rt_test_env`, see [rt_test_exercise].\cr
#' Remember to use [list2env()] on the first call, see [rt_warn] examples.
#' @return If `id`, `lang` or no arguments are given, list with the env content.\cr
#'         If `pass` or `fail` are given, TRUE or FALSE respectively.\cr
#'         Later arguments are ignored if a prior argument is given.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021, Jan 2022
#' @seealso [rt_test_exercise], which defines the env at the start
#'          and [rt_warn] with some examples
#'
#' @param id   Number or charstring to be set as task ID for [rt_warn] messages.
#'             DEFAULT: NULL
#' @param lang Charstring with language for [rt_warn] messages.
#'             Currently, "en" and "de" are implemented. DEFAULT: NULL
#' @param pass Number of task(s) to set as successfull. DEFAULT: NULL
#' @param fail Number of task(s) to set as failed. DEFAULT: NULL
#' @param info Free-form information to be added. Used in rt_run_script. DEFAULT: NULL
#'
rt_env <- function(
id=NULL,
lang=NULL,
pass=NULL,
fail=NULL,
info=NULL
)
{
empty <- list(id="nid", success=vector(), lang=rt_default_language, info="")
env <- dynGet("rt_test_env", ifnotfound=empty, minframe=0)

if(!is.null(id))   {env$id <- id; return(as.list(env))}
if(!is.null(lang)) {if(!lang %in% c("en","de")) stop("lang must be 'en' or 'de', not '",lang,"'.")
	                  env$lang <- lang; return(as.list(env))}
if(!is.null(pass)) {if(is.numeric(pass)) env$success[pass] <- TRUE  ; return(TRUE )}
if(!is.null(fail)) {if(is.numeric(fail)) env$success[fail] <- FALSE ; return(FALSE)}
if(!is.null(info)) {env$info <- c(env$info,info); return(as.list(env))}

# return for empty rt_env() call:
as.list(env)
}

# non-exported package-global default language setting:
rt_default_language <- "de" # for RMOOC March 2022
# Student: use codeoceanR:::rt_env(lang="de") or "en" in your script to change the language.
