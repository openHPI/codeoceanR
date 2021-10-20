#' @title check whether object has intended dimension
#' @description Checks [length] for vectors/lists, [nrow]+[ncol] for dataframe, matrix, array
#' @return TRUE or FALSE, depending on whether test is passed or failed
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @export
#' @param obj   Object to be checked
#' @param value Target object - NOT its dimension (for convenience).
#' @param name  [rt_warn] name. DEFAULT: deparse(substitute(obj))
#'
rt_has_dim <- function(obj, value, name=deparse(substitute(obj)))
{
if(length(dim(value))<2) # vector, list, function, table
{
if(length(obj)!=length(value))
  return(rt_warn("'",name,"' must have length ",length(value),", not ",length(obj), "."))
} else # dataframe, matrix, array
{
if(is.null(nrow(obj))) return(rt_warn("'nrow(",name,")' should not be NULL."))
if(is.null(ncol(obj))) return(rt_warn("'ncol(",name,")' should not be NULL."))
if(nrow(obj)!=nrow(value))
  return(rt_warn("'",name,"' must have ",nrow(value)," rows, not ",nrow(obj), "."))
if(ncol(obj)!=ncol(value))
  return(rt_warn("'",name,"' must have ",ncol(value)," columns, not ",ncol(obj), "."))
}
return(TRUE)
}
