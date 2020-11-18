#' @title test condition
#' @description Test a condition, calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether condition is TRUE.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param condition Generic custom condition to be tested
#' @param \dots     Message passed to [rt_warn] if condition evaluates to FALSE
#'
rt_test <- function(condition, ...){
  if(condition) return(TRUE)
  rt_warn(...)
  FALSE
}
