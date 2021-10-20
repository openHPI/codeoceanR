#' @title Test class of object
#' @return TRUE / FALSE
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @export
#' @param object   Object. Regular object name, not quoted.
#' @param class    Class(es) that are acceptable. Test is passed if any of the classes matches.
#'                 Test is skipped if `class="any"`.
#' @param name     [rt_warn] name. DEFAULT: deparse(substitute(obj))
#'
rt_has_class <- function(object, class, name=deparse(substitute(object)))
{
if(identical(class, "any")) return(TRUE)
if(any(class(object) %in% class)) return(TRUE)

rt_warn("'", name,"' should be ", paste(class, collapse=" or "), ", not of class '", toString(class(object)), "'.")
}
