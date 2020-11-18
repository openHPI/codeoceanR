#' @title test object value
#' @description Test whether object contains certain value(s), calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether object contains `value`.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [rt_has_value], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param obj    Object to be tested. Just the name, not a character string.
#' @param value  value `obj` should have, can be char / numeric / other.
#' @param fixed  Fixed match in [grepl()]? DEFAULT: TRUE
#'
rt_contains <- function(obj, value, fixed=TRUE){
  objname <- deparse(substitute(obj))
  jup <- if(is.character(obj)) grepl(pattern=value, x=obj, fixed=fixed) else
                               value %in% obj
  if(jup) return(TRUE)
  rt_warn(objname, " does not contain ", toString(value))
  FALSE
}
