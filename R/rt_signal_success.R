taskenvironment <- new.env()
taskenvironment$success <- NULL

#' @title Indicate a task succeeded
#' @description Add `tnumber` to the vector `taskenvironment$success` to indicate a task succeeded.
#' @return TRUE, so that it an be used after the last "test" in a task test suite.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_succeeded], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords test
#' @export
#'
#' @param tnumber Task number, preferably numeric.
#'                Can likely also be a charstring, if all [rt_succeeded()] calls check for a character.
#'
rt_signal_success <- function(tnumber)
{
taskenvironment$success <- c(taskenvironment$success, tnumber)
return(TRUE)
}
