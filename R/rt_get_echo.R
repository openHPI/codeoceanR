#' @title get echo from code
#' @description get echoed output from running certain code, e.g. messages + [cat()] outputs.
#' @return Charstring vector from [readLines()]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2020
#' @export
#' @examples
#'
#' rt_get_echo(cat("things\nwith linebreaks"))
#'
#' @param expr Code to be executed while output is collected.
#'             Can be contained within \{curly brackets\}.
#'
rt_get_echo <- function(expr)
{
sinkfile <- tempfile(fileext=".txt")
# Open capturing:
con <- file(sinkfile, open="wt")
sink(con, type="output" , append=TRUE)
sink(con, type="message", append=TRUE)
# evaluate the expression:
expr
# Close capturing:
sink(type="message")
sink()
close(con)
# Output:
return(readLines(sinkfile, warn=FALSE))
}

