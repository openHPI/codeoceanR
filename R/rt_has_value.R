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
#' @param digits Tolerance - both `obj` and `value` are [round]ed before comparison,
#'               if target `value` is numeric. DEFAULT: 6
#' @param name   Object name for [rt_warn] messages. DEFAULT: `deparse(substitute(obj))`
#' @param noise  Add noise, so not the exact difference is reported?
#'               Only used if `value` is numeric. DEFAULT: FALSE
#' @param stepwise Compare `obj` and `value` stepwise?
#'               For numerical objects: only used if noise=FALSE.
#'               DEFAULT: NULL, meaning `length(obj)>1`

rt_has_value <- function(
	obj, value, digits=6,
	name=deparse(substitute(obj)),
	noise=FALSE,
	stepwise=NULL
	){
  force(name)
  if(isTRUE(all.equal(obj,value))) return(TRUE)
	if(is.null(stepwise)) stepwise <- length(obj)>1
	# Reduce message line breaks e.g. in intended try() error messages:
	if(is.character(obj) && length(obj)==1)
		{
		obj   <- sub("\n$", "", obj)
		value <- sub("\n$", "", value)
	  }
  if(is.numeric(value))
  	{
    obj <- round(obj, digits)
    value <- round(value, digits)
    if(noise) return(rt_warn("'",name,"' has the wrong value. The deviance ",
  		"(with added noise) is: ", toString(round(obj-value+rnorm(length(obj)), digits))))
    }
  if(!stepwise) return(rt_warn("'", name, "' should be '", toString(value),
  														 "', not '", toString(obj),"'"))
  # stepwise check:
  for(i in seq_along(obj))
  {
  v <- value[i]
  o <- obj[i]
  neq <- if(is.na(v)|is.na(o)) !isTRUE(all.equal(o,v)) else o!=v
  if(neq) return(rt_warn("'", name, "[",i,"]' should be '",
  												toString(v),"', not '", toString(o),"'"))
  }
	return(TRUE) # e.g. if obj has names and value doesn't, all.equal has not caught it
}
