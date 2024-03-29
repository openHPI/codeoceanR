#' @title Does code produce a wanted echo ([message] + [cat] + [print]), warning or error?
#' @return TRUE / FALSE by default, the result of running the code if `value=TRUE`,
#'         the captured messages themselves if `msg=TRUE`.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2020 + Oct 2021
#' @seealso [capture.output], [try], \url{https://stackoverflow.com/a/4947528/1587132}
#' @export
#' @examples
#' # captured messages:
#' rt_gives("echo", {77; 88; 99}, value=TRUE)           # 99
#' rt_gives("echo", cat("line\nwith break"), capt=TRUE) # "line\\nwith break"
#' rt_gives("echo", message("some message"), capt=TRUE) # "some message"
#' rt_gives("echo", {77; 88; 99}, capt=TRUE)            # ""
#' rt_gives("echo", {77; print(88); 99}, capt=TRUE)     # "[1] 88"
#' rt_gives("echo", {log(-77); print(99)}, capt=TRUE)   # warning not in echo
#' rt_gives("echo", {print(99) ; log("6")}, capt=TRUE)  # error is included
#' rt_gives("warning",{log(-4);log("6");log(-3)},capt=TRUE) # error is included with keyword CAPTURED
#' rt_gives("warning",{log(-4);log(678);log(-3)},capt=TRUE) # both warnings
#' rt_gives("error", log("3"), capt=TRUE)               # "Error in log(\"3\") : non-numeric [...]
#'
#' # Testing messages:
#' rt_gives("echo", {print(77); cat("aa bb")}, "aa b") # TRUE
#' rt_gives("echo", cat("aa bb"), "aa b", exact=TRUE)  # wrong echo
#' rt_gives("echo", {print(77); cat("stuff")}, "thing")# wrong echo
#' rt_gives("echo", log(7), "")                        # TRUE
#' rt_gives("echo", cat(88), "")                       # shouldn't echo but gives '88'
#' rt_gives("echo", log(7), "stuff")                   # should echo something

#' rt_gives("warning", log(-7), "NaNs produced")       # TRUE
#' rt_gives("warning", log( 7), "NaNs produced")       # should warn
#' rt_gives("warning", log(-7), "")                    # shouldn't warn, but gives ...
#' rt_gives("warning", log(7), "")                     # TRUE
#'
#' rt_gives("error", log(777), "")                     # TRUE
#' rt_gives("error", log("7"), "")                     # Shouldn't fails but raises ...
#' rt_gives("error", log("7"), "non-numeric argument") # TRUE
#' rt_gives("error", log(777), "non-numeric argument") # should raise error
#'
#' try(rt_gives("dummy", log("7"), "") ) # error: method 'dummy' is not implemented.
#'
#' # convenience testing:
#' rt_gives("error", log(7),   expr2=log("7"))         # should raise error
#' rt_gives("error", log("7"), expr2=log("8"))         # TRUE (call is excluded from msg)
#' rt_gives("error", log("7"), expr2=notexist)         # should yield ..., not ...
#' rt_gives("echo",  cat(7),   expr2=cat(8))           # should echo '8', not '7'
#' rt_gives("error", log("7"), expr2=log(7))           # should not fail, but raises ...
#' rt_gives("error", log(7),   expr2=log(7))           # TRUE
#' rt_gives("error", log("7"), expr2=log("7"))         # TRUE
#'
#' @param type  Charstring, one of "echo", "warning", "error".
#' @param expr  Code to be executed while side-output is collected.
#'              Can be contained within \{curly brackets\}.
#' @param msg   Desired echo/warning/error message.
#'              DEFAULT: `""` (test if no message is generated).
#' @param expr2 If given, msg will be determined from running this code. DEFAULT: NULL
#' @param value Return the result of running `expr`? DEFAULT: FALSE
#' @param capt  Return the captured echo/warnings/error of running `expr`
#'              (instead of testing them)? DEFAULT: FALSE
#' @param exact use [==] instead of [grepl]? Useful e.g. for type="echo" to see if
#'              message is used, not print. DEFAULT: FALSE
#' @param fixed Treat `msg` as is, rather than regex? (only if exact=FALSE) DEFAULT: TRUE
#' @param name  Charstring to be included in testing messages. DEFAULT: input code
#'
rt_gives <- function(
type,
expr,
msg="",
expr2=NULL,
value=FALSE,
capt=FALSE,
exact=FALSE,
fixed=TRUE,
name=deparse(substitute(expr))
){
force(name)
out <- switch(type,
       echo   =rt_gives_echo(expr),    # see unexported function definitions below
       warning=rt_gives_warning(expr),
       error  =rt_gives_error(expr),
       stop("method '",type,"' is not implemented.")
       )
if(value) return(out$value)
if(capt)  return(out$captured)
if(deparse(substitute(expr2))!="NULL") msg <- switch(type,
                           echo   =rt_gives_echo   (expr2)$captured,
                           warning=rt_gives_warning(expr2, call=FALSE)$captured,
                           error  =rt_gives_error  (expr2, call=FALSE)$captured)
captu <- out$captured

if(rt_env()$lang=="de")
{
# no message when needed:
wpart <- switch(type,
         echo   = " etwas ausgeben (z.B. durch message, cat, print).",
         warning= " eine Warnung erzeugen.",
         error  = " einen Fehler erzeugen.")
if(captu=="" && msg!="") return(rt_warn("'",name,"' sollte",wpart))
# message when should be silent:
wpart <- switch(type,
         echo   = " nichts ausgeben, aber gibt '",
         warning= " nicht warnen, aber erzeugt die Warnung '",
         error  = " nicht versagen, aber erzeugt den Fehler '")
if(captu!="" && msg=="") return(rt_warn("'",name,"' sollte",wpart,captu,"'."))
# message different than needed:
iscor <- if(exact) msg==captu else grepl(msg, captu, fixed=fixed)
if(!iscor){
wpart <- switch(type,
         echo   = paste0("'", msg, "' ausgeben, nicht '"),
         warning= paste0("die Warnung '", msg, "' erzeugen, nicht '"),
         error  = paste0("den Fehler '", msg, "' erzeugen, nicht '"))
return(rt_warn("'",name,"' sollte ",wpart, captu,"'."))}
}
else
{
# no message when needed:
wpart <- switch(type,
         echo   = " echo something (e.g. through message, cat, print).",
         warning= " yield a warning.",
         error  = " raise an error.")
if(captu=="" && msg!="") return(rt_warn("'",name,"' should",wpart))
# message when should be silent:
wpart <- switch(type,
         echo   = " echo anything, but gives '",
         warning= " warn, but gives '",
         error  = " fail, but raises the error '")
if(captu!="" && msg=="") return(rt_warn("'",name,"' should not",wpart,captu,"'."))
# message different than needed:
wpart <- switch(type,
         echo   = " echo '",
         warning= " generate the warning '",
         error  = " yield the error '")
iscor <- if(exact) msg==captu else grepl(msg, captu, fixed=fixed)
if(!iscor) return(rt_warn("'",name,"' should",wpart,msg,"', not '",captu,"'"))
}
# pass:
TRUE
}



rt_gives_echo <- function(expr)
{
sinkfile <- tempfile(fileext="_co_echo.txt")
# Open capturing:
con <- file(sinkfile, open="wt")
sink(con, type="output" , append=TRUE)
sink(con, type="message", append=TRUE)
# evaluate the expression:
value <- try(expr, silent=TRUE)
if(inherits(value, "try-error")) cat(value)
# Close capturing:
sink(type="message")
sink()
close(con)
# Output:
captured <- readLines(sinkfile, warn=FALSE)
list(value=value, captured=paste(captured, collapse="\\n"))
}


rt_gives_warning <- function(expr, call=TRUE)
{
wlist <- NULL
wfun <- function(e)
	{
	ccall <- deparse(conditionCall(e))
	if(ccall=="withCallingHandlers(expr, warning = wfun)") ccall <- "unknown_call"
	ccall <- paste0(ccall,": ")
  wlist <<- c(wlist, paste0(if(call) ccall, conditionMessage(e)))
  invokeRestart("muffleWarning")
  }
value <- try(withCallingHandlers(expr, warning=wfun), silent=TRUE)
if(inherits(value, "try-error")) wlist <- c(wlist, paste0("CAPTURED: ", value))
list(value=value, captured=paste(wlist, collapse=if(call) "\\n" else ".*"))
}


rt_gives_error <- function(expr, call=TRUE)
{
err <- try(expr, silent=TRUE)
iserror <- inherits(err, "try-error")
value <- if(!iserror) err else NULL
if(!call) err <- attr(err,"condition")$message
err   <- if(!iserror) "" else gsub("\n", "\\n", sub("\n$","",err[1]))
list(value=value, captured=err)
}
