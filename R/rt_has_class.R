#' @title Test class of object
#' @return TRUE / FALSE
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @export
#' @param object   Object. Regular object name, not quoted.
#' @param class    Class(es) that are acceptable. Test is passed if any of the classes matches.
#'                 Test is skipped if `class="any"`.
#' @param name     [rt_warn] name. DEFAULT: deparse(substitute(obj))
#' @param matarr   Accept (and message) only 'matrix' if `class` is c("matrix","array")?
#'                 DEFAULT: TRUE
#' @param intnum   Should integer and numeric both be OK if `class` is one of those?
#'                 DEFAULT: TRUE
#'
rt_has_class <- function(object, class, name=deparse(substitute(object)), matarr=TRUE, intnum=TRUE)
{
if(identical(class, "any")) return(TRUE)
if(matarr) if(identical(class, c("matrix","array"))) class <- "matrix"
if(intnum) if(identical(class, "integer")||identical(class, "numeric")) class <- c("integer","numeric")
if(any(class(object) %in% class)) return(TRUE)

rt_warn("'", name,"' should have class '", paste(class, collapse="' or '"), "', not '", toString(class(object)), "'.")
}
