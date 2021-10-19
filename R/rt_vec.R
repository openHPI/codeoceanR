#' @title Convert vectors to charstring code representations
#' @return single charstring
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @seealso [dput], [toString]
#' @keywords character
#' @export
#' @examples
#' rt_vec(pi)
#' rt_vec("hey")
#' message(rt_vec("hey"))
#' message(rt_vec(letters[1:6]))
#' message(rt_vec(sample(1:90, 7)))
#' @param x input vector
#'
rt_vec <- function(x)
{
ll <- length(x)!=1
ch <- is.character(x)
col <- if(ch) '","' else ','
paste0(if(ll)'c(',if(ch)'"', paste(x,collapse=col),if(ch)'"',if(ll)")")
}
