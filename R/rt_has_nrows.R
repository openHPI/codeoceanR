#' @title test object dimension
#' @description Test number of rows of object, calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether object has `len` rows.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [rt_has_ncols], [rt_has_length], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param df     data.frame-like object to be tested. Just the name, not a character string.
#' @param len    Value to which [nrow] is compared
#'

rt_has_nrows <- function(df, len) {if(nrow(df)==len) return(TRUE)
  rt_warn(deparse(substitute(df)), " must have ", len, " rows, not ", nrow(df), ".")
  FALSE
}
