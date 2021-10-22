#' @title Test whether an expression yields a specific warning
#' @return TRUE or FALSE
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @seealso [rt_gives_echo] \url{https://stackoverflow.com/a/4947528/1587132}
#' @export
#' @examples
#' rt_test_env <- new.env()
#' rt_gives_warning({log(-7); as.numeric("a")}, "NaNs produced") # TRUE
#' rt_gives_warning({log( 7); as.numeric("a")}, "NaNs produced") # wrong warning
#' rt_gives_warning({log(-7); as.numeric("a")}, "") # if no warning was desired
#' rt_gives_warning(log(7), "") # TRUE (no warning)
#' rt_gives_warning(log(-7), "") # shouldn't warn but does
#' rt_gives_warning(log(-7), NULL) # TRUE (generates any warning)
#' rt_gives_warning(log(7), NULL) # should warn but doesn't
#' @param expr Expression to be evaluated
#' @param w    Charstring: warning that needs to be present (matched with grepl).
#'             Use `w=""` to test if no warnings are generated.
#'             Use `w=NULL` to test if any warning is produced.
#'             DEFAULT: NULL
#'
rt_gives_warning <- function(expr, w=NULL)
{
msg <- deparse(substitute(expr))
wlist <- NULL

# Run expression, capturing warnings:
wfun <- function(e)
	{
	ccall <- deparse(conditionCall(e))
	if(ccall=="withCallingHandlers(expr, warning = wfun)") ccall <- "unknown_call"
  wlist <<- c(wlist, paste0(ccall,": ",conditionMessage(e)))
  invokeRestart("muffleWarning")
  }
val <- withCallingHandlers(expr, warning=wfun)

# Test for any warning:
if(is.null(w)) if(!is.null(wlist)) return(TRUE) else
	return(rt_warn("'", msg, "' should yield a warning."))

# Test if there is no warning:
if(w=="") if(is.null(wlist)) return(TRUE) else
	return(rt_warn("'", msg, "' should not yield any warning but gives: ", toString(wlist)))

# Test for specific warning message:
if(any(grepl(w, wlist))) return(TRUE)
rt_warn("'", msg, "' should yield the warning '", w, "', not: ", toString(wlist))
}


