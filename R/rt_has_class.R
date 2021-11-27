#' @title Test class of object
#' @return TRUE / FALSE
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @export
#' @param object   Object. Regular object name, not quoted.
#' @param class    Class(es) that are acceptable. Test is passed if any of the classes matches.
#'                 Test is skipped if `class="any"`.
#' @param name     [rt_warn] name. DEFAULT: deparse(substitute(obj))
#' @param qmark    Include ' marks around `name`? DEFAULT: TRUE
#' @param matarr   Accept (and message) only 'matrix' if `class` is c("matrix","array")?
#'                 DEFAULT: TRUE
#' @param intnum   Should integer and numeric both be OK if `class` is one of those?
#'                 DEFAULT: TRUE
#'
rt_has_class <- function(
object,
class,
name=deparse(substitute(object)),
qmark=TRUE,
matarr=TRUE,
intnum=TRUE
)
{
if(identical(class, "any")) return(TRUE)
force(name)
if(matarr) if(identical(class, c("matrix","array"))) class <- "matrix"
class2 <- class
if(intnum) if(identical(class, "integer")||identical(class, "numeric"))
	class2 <- c("integer","numeric")
if(any(class(object) %in% class2)) return(TRUE)

rt_warn(if(qmark)"'",name,if(qmark)"'"," should have class '",
				paste(class, collapse="' or '"), "', not '", toString(class(object)), "'.")
}
