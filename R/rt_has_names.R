#' @title test object names
#' @description Test names of object, calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether object has the desired names.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [rt_has_column], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param obj    Object to be tested. Just the name, not a character string.
#' @param n      Charstring to which [names] is compared
#'
rt_has_names <- function(obj, n) {if(all(n %in% names(obj))) return(TRUE)
  rt_warn(deparse(substitute(x)), " must have the name", if(length(n)>1) "s:", " ", toString(n), ".")
  FALSE
}
