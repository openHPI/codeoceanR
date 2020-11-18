#' @title test object class
#' @description Test class of object, calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether `obj` [inherits] the desired [class].
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param obj    Object to be tested. Just the name, not a character string.
#' @param class  Charstring passed to [inherits]
#'
rt_has_class <- function(obj, class) {
  if(inherits(obj, class)) return(TRUE)
  rt_warn(deparse(substitute(obj)), " must be '", class, "', not of class '", toString(class(obj)), "'.")
  FALSE
}
