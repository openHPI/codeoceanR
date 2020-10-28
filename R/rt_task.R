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



#' @title Test whether a (previous) task succeeded
#' @description Test whether `tnumber` is in the vector `taskenvironment$success`.
#' @return TRUE or FALSE, depending on `taskenvironment$success` content.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_signal_success], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords test
#' @export
#'
#' @param tnumber Task number, see [rt_signal_success]
#' @param msg     Charsting with message to be prefixed to `tnumber`.
#'
rt_succeeded <- function(tnumber, msg="Please first solve task ")
{
if(tnumber %in% taskenvironment$success) return(TRUE)
rt_warn(msg, tnumber)
return(FALSE)
}
