#' @title test object existence
#' @description Test existence of object, calling [rt_warn()] with informative and helpful message if needed.
#' @return Logical: TRUE / FALSE depending on whether object exists.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords error test
#' @seealso [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param obj    Object to be tested. Just the name, not a character string.
#'
rt_exists <- function(obj) {
  if(!is.character(obj)) obj <- deparse(substitute(obj)) # charstring of object name
  if(base::exists(obj)) return(TRUE)
  rt_warn("Create the object '",obj,"'. It does not yet exist.")
  FALSE
}
