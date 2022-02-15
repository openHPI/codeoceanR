#' @title warn about errors
#' @description pass test messages to Rstudio / CodeOcean.
#' @return FALSE and console printout of [message()] in interactive mode (RStudio) or
#'                   [cat()] otherwise (CodeOcean)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords IO error
#' @seealso [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#' @examples
#' rt_warn("This is a CodeOcean message!")
#' rt_warn("This is message Nr ", 2+6, ".")
#' rt_env <- codeoceanR:::rt_env # not exported to keep UI clean.
#' rt_test_env <- list2env(rt_env(lang="en"))
#' rt_warn(de="de1 ", en="en1 ", 77, de=" de2", en=" en2", "'", min(longley))
#' invisible(rt_env(lang="de"))
#' rt_warn(de="de1 ", en="en1 ", 77, de=" de2", en=" en2", "'", min(longley))
#' # rt_env(lang="DE") # error message
#' rt_warn("Part1 ", if(TRUE) c(de="de3", en="en3"), " closing.") # both included.
#' cond <- TRUE
#' rt_warn("P1 ", de=if(cond)"de3 ", en=if(cond)"en3 ", "end.")
#' cond <- FALSE
#' rt_warn("P1 ", de=if(cond)"de3 ", en=if(cond)"en3 ", "end.")
#' rm(rt_test_env, rt_env)
#'
#' @param \dots Message components passed to [warning()] or [cat()].
#'              Can be named de or en to only be shown for a specific language,
#'              see the examples
#'
rt_warn <- function(...){
	m <- list(...)
	n <- names(m)
	m <- if(is.null(n)) m else m[n %in% c(rt_env()$lang, "")]
	if(interactive())
              message("T", rt_env()$id, ": ", unlist(m),       sep="") else
  cat("AssertionError: T", rt_env()$id, ": ", unlist(m), "\n", sep="")
  return(FALSE)
}
