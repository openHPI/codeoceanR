#' @title test object dimension
#' @description Test length of object, calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether object has the desired length.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param obj    Object to be tested. Just the name, not a character string.
#' @param len    Value to which [length] is compared
#'
rt_has_length <- function(obj, len) {if(length(obj)==len) return(TRUE)
  rt_warn(deparse(substitute(obj)), " must have length ", len,  ", not ", length(obj), ".")
  FALSE
}
