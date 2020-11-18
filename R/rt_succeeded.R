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
