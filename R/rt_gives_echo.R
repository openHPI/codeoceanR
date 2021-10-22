#' @title Does running code produce a wanted echo ([message] + [cat] + [print] outputs)?
#' @return TRUE / FALSE, Charstring vector from [readLines()]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2020 + Oct 2021
#' @seealso [rt_gives_warning] [capture.output()]
#' @export
#' @examples
#' rt_test_env <- new.env()
#' rt_gives_echo(cat("things\nwith linebreaks"), echo=TRUE)
#' rt_gives_echo(message("some message"), echo=TRUE)
#' rt_gives_echo({77; 88; 99}, echo=TRUE) # empty
#' rt_gives_echo({77; 88; 99}, value=TRUE) # 99
#' rt_gives_echo({77; print(88); 99}, echo=TRUE) # "[1] 88"
#' rt_gives_echo({print(77); log(-6); print(99)}, echo=TRUE) # warning not in echo
#' rt_gives_echo({print(77); log("6"); print(99)}, echo=TRUE) # error is included
#'
#' rt_gives_echo({print(77); message("some stuff")}, "e stuff") # TRUE
#' rt_gives_echo({print(77); message("some stuff")}, "more stuff") # wrong echo
#' rt_gives_echo(log(7), "") # TRUE (no echo)
#' rt_gives_echo(cat(88), "") # shouldn't echo but does
#' rt_gives_echo(cat(999), NULL) # TRUE (generates any echo)
#' rt_gives_echo(log(7), NULL) # should echo something (e.g. through ...) but doesn't
#' rt_gives_echo(log(7), "stuff") # should echo something
#' rt_gives_echo(cat(7), "stuff") # wrong echo
#'
#' @param expr  Code to be executed while output is collected.
#'              Can be contained within \{curly brackets\}.
#' @param e     Charstring: echo that needs to be present (matched with [grepl]).
#'              Use `e=""` to test if no echo is generated.
#'              Use `e=NULL` to test if any echo is produced.
#'              DEFAULT: NULL
#' @param value Return the result of running `expr`? DEFAULT: FALSE
#' @param echo  Return the echo of running `expr` (instead of testing it)? DEFAULT: FALSE
#' @param fixed If tested for specific e with [grepl], treat charstring `e` as is,
#'              rather than regex? DEFAULT: TRUE
#' @param exact use == instead of grepl? Useful to see if message is used, not print.
#'              DEFAULT: FALSE
#' @param msg   Charstring to be included in testing messages. DEFAULT: input code
#'
rt_gives_echo <- function(
expr,
e=NULL,
value=FALSE,
echo=FALSE,
fixed=TRUE,
exact=FALSE,
msg=deparse(substitute(expr))
){
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
if(value) return(val)
captured <- readLines(sinkfile, warn=FALSE)
if(echo) return(captured)

# Test for any echo:
if(is.null(e) && length(captured)!=0) return(TRUE)
if(!identical(e,"") && length(captured)==0)
  return(rt_warn("'", msg, "' should echo something (e.g. through message, cat, print)."))

# Test if there is no echo:
if(e=="") if(length(captured)==0) return(TRUE) else
	return(rt_warn("'", msg, "' should not echo anything but gives: '", paste(captured, collapse="\\n"),"'"))

# Test for specific echo:
iscor <- if(exact) e %in% captured else any(grepl(e, captured, fixed=fixed))
if(iscor) return(TRUE)
rt_warn("'", msg, "' should echo '", e, "', not '", paste(captured, collapse="\\n"),"'")

}

