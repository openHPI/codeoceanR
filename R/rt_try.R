rt_try <- function(expr) # simplified version of berryFunctions::tryStack
{
oop <- options(warn=-1)
on.exit(options(oop), add=TRUE)
# environment for stack to (potentially) be written into:
tryenv <- new.env()
# error/warning function
efun <- function(e, iswarn=FALSE)
{
stack <- sys.calls()
stack <- sapply(stack, function(x) paste(deparse(x), collapse="\n"))
ign <- c(
	# errors
	"h(simpleError(msg, call))",
	".handleSimpleError(function (e, iswarn = FALSE)", # incomplete, this function itself
	"withCallingHandlers(expr, error = efun, warning = wfun)", # this fun
	"doTryCatch(return(expr), name, parentenv, handler)",
	"tryCatchOne(expr, names, parentenv, handlers[[1L]])",
	"tryCatchList(expr, classes, parentenv, handlers)",
	"tryCatch(expr, error = function(e) {\n    call <- c", # incomplete
	"try(withCallingHandlers(expr, error = efun, warnin", # incomplete
	"eval(ei, envir)",
	"withVisible(eval(ei, envir))",
	# warnings
	".signalSimpleWarning(",
	"withRestarts({\n    .Internal(.signalCondition",
	"withOneRestart(expr, restarts[[1L]])",
	"doWithOneRestart(return(expr), restart)",
	"(function (e) \nefun(e, iswarn = TRUE))(structure", # this fun
	# testing stuctures
	"rt_test_exercise({",
	"rt_try(eval(substitute(expr)))",
	"eval(substitute(expr))"
	)
for(i in ign) stack <- stack[!startsWith(stack, i)]
# add code that produced error/warning:
stack <- c(stack, deparse(conditionCall(e))[1L]  )
# prepend numbers:
stack <- paste0(seq_along(stack), ": ", stack)
# concatenate:
stack <- paste(stack, collapse="\n"  )
# add error/warning message:
cmes <- conditionMessage(e)
cmes <-  sub("\n+$", "", cmes)
cmes <- paste0(if(iswarn) "Warning: " else "Error: ", cmes)
stack <- paste0(stack,"\n", cmes)
# add message to main function environment:
tryenv$msg <- c(tryenv$msg, stack)
} # efun end
wfun <- function(e) efun(e, iswarn=TRUE)
# now try the expression:
out <- try(withCallingHandlers(expr, error=efun, warning=wfun), silent=TRUE)
# add the trace stack character string to the output:
if(T||inherits(out, "try-error")) out[1] <- paste(tryenv$msg, collapse="\n\n")
return(invisible(out))
}
