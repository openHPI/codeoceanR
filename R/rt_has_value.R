#' @title test object value
#' @description Test values of object, calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether object has the given value.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [rt_contains], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#' @importFrom stats rnorm
#'
#' @param obj    Object to be tested. Just the name, not a character string.
#' @param value  value `obj` should have, can be char / numeric / other.
#' @param digits Tolerance - both `obj` and `value` are [round]ed before comparison. DEFAULT: 6
#' @param name   Object name for [rt_warn] messages. DEFAULT: `deparse(substitute(obj))`
#' @param noise  Add noise, so not the exact difference is reported? DEFAULT: TRUE

rt_has_value <- function(obj, value, digits=6, name=deparse(substitute(obj)), noise=TRUE){
  force(name)
  if(is.character(obj)) if(all(obj==value)) return(TRUE) else
    {rt_warn("'", name, "' should be  '", toString(value), "'  but is  '", toString(obj), "'.");
    return(FALSE)}

  if(  all(is.na(value)) & all(is.na(obj))  ) return(TRUE)

  l <- length(obj)
  n <- sum(is.na(obj))
  if(n>0) {rt_warn("'", name, "'", if(l==1) " is NA." else paste0(" has NAs", " (", n, "/", l, ").")) ; return(FALSE)}

  obj <- round(obj, digits)
  val <- round(value, digits)
  dif <- obj - val
  if(noise) dif <- dif + round(rnorm(length(obj)),6)
  dif <- round(dif, digits)
  dif <- paste0("The deviance is",if(noise)" (approximately, with added noise)", ": ", toString(dif))
  if(all(obj==val)) return(TRUE)
  rt_warn("'", name, "' has the wrong value. ", dif)
  FALSE
}
