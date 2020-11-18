#' @title test object column
#' @description Test colnames of object, calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether object has the desired column.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [rt_has_ncols], [rt_has_names], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param df     data.frame-like object to be tested. Just the name, not a character string.
#' @param column Charstring to which [colnames] is compared
#'
rt_has_column <- function(df, column) {if(column %in% colnames(df)) return(TRUE)
  rt_warn(deparse(substitute(df)), " must have the column ", column, ".")
  FALSE
}
