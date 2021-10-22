#' @title Does running code produce a wanted echo ([message] + [cat] + [print] outputs)?
#' @return TRUE / FALSE, Charstring vector from [readLines()]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2020 + Oct 2021
#' @seealso [rt_gives_warning] [capture.output()]
#' @export
#' @examples
#' rt_test_env <- new.env()
#' rt_gives_echo(cat("things\nwith linebreaks"), value=TRUE)
#' rt_gives_echo(message("some message"), value=TRUE)
#' rt_gives_echo({77; 88; 99}, value=TRUE) # empty
#' rt_gives_echo({77; print(88); 99}, value=TRUE) # "[1] 88"
#' rt_gives_echo({print(77); log(-6); print(99)}, value=TRUE) # warning not in echo
#' rt_gives_echo({print(77); log("6"); print(99)}, value=TRUE) # error is included
#'
#' rt_gives_echo({print(77); message("some stuff")}, "e stuff") # TRUE
#' rt_gives_echo({print(77); message("some stuff")}, "more stuff") # wrong echo
#' rt_gives_echo(log(7), "") # TRUE (no echo)
#' rt_gives_echo(cat(88), "") # shouldn't echo but does
#' rt_gives_echo(cat(999), NULL) # TRUE (generates any echo)
#' rt_gives_echo(log(7), NULL) # should echo something but doesn't
#'
#' @param expr  Code to be executed while output is collected.
#'              Can be contained within \{curly brackets\}.
#' @param e     Charstring: echo that needs to be present (matched with grepl).
#'              Use `e=""` to test if no echo is generated.
#'              Use `e=NULL` to test if any echo is produced.
#'              DEFAULT: NULL
#' @param value Return the echo instead of testing it? DEFAULT: FALSE
#' @param msg   Charstring to be included in testing messages. DEFAULT: input code
#'
rt_gives_echo <- function(expr, e=NULL, value=FALSE, msg=deparse(substitute(expr)))
{
force(msg)

sinkfile <- tempfile(fileext="_co_echo.txt")
# Open capturing:
con <- file(sinkfile, open="wt")
sink(con, type="output" , append=TRUE)
sink(con, type="message", append=TRUE)
# evaluate the expression:
val <- try(expr, silent=TRUE)
if(inherits(val, "try-error")) cat(val)
# Close capturing:
sink(type="message")
sink()
close(con)
# Output:
echo <- readLines(sinkfile, warn=FALSE)
if(value) return(echo)

# Test for any echo:
if(is.null(e)) if(length(echo)!=0) return(TRUE) else
	return(rt_warn("'", msg, "' should yield an echo."))

# Test if there is no echo:
if(e=="") if(length(echo)==0) return(TRUE) else
	return(rt_warn("'", msg, "' should not yield any echo but gives: '", paste(echo, collapse="\\n"),"'"))

# Test for specific echo:
if(any(grepl(e, echo))) return(TRUE)
rt_warn("'", msg, "' should yield the echo '", e, "', not '", paste(echo, collapse="\\n"),"'")

}

