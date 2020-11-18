#' @title test object identity
#' @description Test whether two objects are identical, calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether objects are identical.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param obj    Object to be tested. Just the name, not a character string.
#' @param target Object to be compared to via [identical]
#'
rt_is_identical <- function(obj, target){
  if(identical(obj,target)) return(TRUE)
  rt_warn(deparse(substitute(obj)), " should be ", toString(target), " but is ", toString(obj))
  FALSE
}
