#' @title warn about errors
#' @description pass test messages to Rstudio / CodeOcean.
#' @return invisible FALSE and console printout of [message()] in interactive mode (RStudio) or
#'                   [cat()] otherwise (CodeOcean)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords IO error
#' @seealso [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#' @examples
#' rt_warn("This is a CodeOcean message!")
#'
#' @param \dots Message components passed to [warning()] or [cat()].
#'
rt_warn <- function(...){
	if(interactive())
              message("T", rt_env()$id, ": ", ...,       sep="") else
  cat("AssertionError: T", rt_env()$id, ": ", ..., "\n", sep="")
  return(invisible(FALSE))
}
