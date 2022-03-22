#' @title test object value
#' @description Test values of object, calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether object has the given value.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [rt_contains], [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#' @importFrom stats rnorm
#'
#' @param object Object to be tested. Just the name, not a character string.
#' @param value  value `object` should have, can be char / numeric / other.
#' @param name   Object name for [rt_warn] messages. DEFAULT: `deparse(substitute(object))`
#' @param qmark  Include ' marks around `name`? DEFAULT: TRUE
#' @param digits Tolerance - both `object` and `value` are [round]ed before comparison,
#'               if target `value` is numeric. DEFAULT: 6
#' @param stepwise Compare `object` and `value` stepwise?
#'               DEFAULT: NULL, meaning `length(value)>1`

rt_has_value <- function(
	object, value,
	name=deparse(substitute(object)),
	qmark=TRUE,
	digits=6,
	stepwise=NULL
	){
  force(name)
  if(isTRUE(all.equal(object,value))) return(TRUE)
	if(is.null(stepwise)) stepwise <- length(value)>1
	# printing name:
	pn <- if(qmark) paste0("'", name, "'") else name
	pn <- paste0(pn, " ")
	# Reduce message line breaks e.g. in intended try() error messages:
	if(is.character(object) && length(object)==1)
		{
		if(!is.null(object)) object <- sub("\n$", "", object)
		if(!is.null(value )) value  <- sub("\n$", "", value)
	  }
  if(is.numeric(value))
  	{
    object <- round(object, digits)
    value  <- round(value , digits)
    }

	toString2 <- function(x) if(is.null(x)) "NULL" else toString(x)

	if(!stepwise) return(rt_warn(pn,en="should be '",de="sollte '",
			toString2(value), en="', not '",de="' sein, nicht '", toString2(object),"'."))
  # stepwise check:
	loop <- if(is.null(value)) 1 else seq_along(value)
  for(i in loop)
  {
  o <- object[[i]] # double square brackets can handle both lists and vectors,
  v <-  value[[i]] # if i is a single value as returned by seq_along
  # list vs vector difference should not be missed (e.g. lapply/sapply output):
  if(!is.null(o)&&!is.null(v)) if(!rt_has_class(o,class(v), paste0(name, "[",i,"]"))) return(FALSE)
  neq <- try(o!=v, silent=TRUE)
  if(is.null(v)) neq <- !is.null(o)
  if(anyNA(neq) || inherits(neq,"try-error") || length(neq)==0) # for NA, lists and other incomparables
  	 neq <- !isTRUE(all.equal(o,v))
  if(any(neq)) return(rt_warn(if(qmark)"'", name, "[",i,"]",if(qmark)"'",
  				en=" should be '",de=" sollte '", toString2(v),
  				en="', not '",de="' sein, nicht '", toString2(o),"'."))
  }
	return(TRUE) # e.g. if object has names and value doesn't, all.equal has not caught it
}
