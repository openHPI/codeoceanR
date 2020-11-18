#' @title test object dimension
#' @description Test ncol of object, calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether object has `len` columns.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [rt_has_column], [rt_has_nrows], [rt_has_length], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param df     data.frame-like object to be tested. Just the name, not a character string.
#' @param len    Value to which [ncol] is compared
#'
rt_has_ncols <- function(df, len) {if(ncol(df)==len) return(TRUE)
  rt_warn(deparse(substitute(df)), " must have ", len, " columns, not ", ncol(df), ".")
  FALSE
}
