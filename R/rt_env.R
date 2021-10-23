#' @title interact with rt_test_env
#' @description interact with rt_test_env, see [rt_test_exercise]
#' @return If `id` or no arguments are given, list with the env content.\cr
#'         If `pass` or `fail` are given, TRUE or FALSE respectively.\cr
#'         Later arguments are ignored if a prior argument is given.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @seealso [rt_test_exercise], which defines the env at the start
#'
#' @param id   Number or charstring to be set as task ID for [rt_warn] messages. DEFAULT: NULL
#' @param pass Number of task(s) to set as successfull. DEFAULT: NULL
#' @param fail Number of task(s) to set as failed. DEFAULT: NULL
#'
rt_env <- function(
id=NULL,
pass=NULL,
fail=NULL
)
{
env <- dynGet("rt_test_env", ifnotfound=list(id="nid", success=vector()), minframe=0)

if(!is.null(id))   {env$id <- id; return(as.list(env))}
if(!is.null(pass)) {stopifnot(is.numeric(pass)); return(env$success[pass] <- TRUE)}
if(!is.null(fail)) {stopifnot(is.numeric(fail)); return(env$success[fail] <- FALSE)}

as.list(env)
}
