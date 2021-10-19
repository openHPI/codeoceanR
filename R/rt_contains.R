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
#' @param ignore_space Remove spaces before comparison? DEFAULT: TRUE
#' @param ignore_quote Replace `'` with `"` before comparison? DEFAULT: TRUE
#'
rt_contains <- function(obj, value, fixed=TRUE, ignore_space=TRUE, ignore_quote=TRUE){
  objname <- deparse(substitute(obj))
  if(!is.character(obj))
    {
    if(value %in% obj) return(TRUE)
    } else
    {
    value2 <- if(ignore_space) gsub("\\s", "", value) else value
    obj2   <- if(ignore_space) gsub("\\s", "", obj  ) else obj
    if(ignore_quote) value2 <- gsub("'", '"', value2)
    if(ignore_quote) obj2   <- gsub("'", '"', obj2  )
    if(any(grepl(pattern=value2, x=obj2, fixed=fixed))) return(TRUE)
    }
  rt_warn("'", objname, "' does not contain '", toString(value), "'")
  FALSE
}
