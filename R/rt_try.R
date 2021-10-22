rt_try <- function(expr) # simplified version of berryFunctions::tryStack
{
# environment for stack to (potentially) be written into:
tryenv <- new.env()
# error/warning function
efun <- function(e)
{
stack <- sys.calls()
stack <- sapply(stack, function(x) paste(deparse(x), collapse="\n"))
ign <- c(
	"h(simpleError(msg, call))",
	".handleSimpleError(function (e)", # incomplete, this function itself
	"withCallingHandlers(expr, error = efun", # this fun
	"doTryCatch(return(expr), name, parentenv, handler)",
	"tryCatchOne(expr, names, parentenv, handlers[[1L]])",
	"tryCatchList(expr, classes, parentenv, handlers)",
	"tryCatch(expr, error = function(e) ", # incomplete
	"try(withCallingHandlers(expr, error = efun", # incomplete
	"eval(ei, envir)",
	"withVisible(eval(ei, envir))",
	# testing stuctures:
	"rt_test_exercise({",
	"rt_try(eval(substitute(expr)))",
	"eval(substitute(expr))"
	)
for(i in ign) stack <- stack[!startsWith(stack, i)]
# add code that produced error:
stack <- c(stack, deparse(conditionCall(e))[1L])
# remove adjacent duplicates: (https://stackoverflow.com/a/27022539)
stack <- stack[ c(stack[-1]!= stack[-length(stack)], TRUE) ]
# prepend numbers:
stack <- paste0(seq_along(stack), ": ", stack)
# concatenate:
stack <- paste(stack, collapse="\n")
# add error message:
cmes <- conditionMessage(e)
cmes <-  sub("\n+$", "", cmes)
cmes <- paste0("Error: ", cmes)
stack <- paste0(stack,"\n", cmes)
# add message to main function environment:
tryenv$msg <- c(tryenv$msg, stack)
} # efun end

# now try the expression:
out <- try(withCallingHandlers(expr, error=efun), silent=TRUE)
# add the trace stack character string to the output:
if(inherits(out, "try-error")) out[1] <- paste(tryenv$msg, collapse="\n\n")
return(invisible(out))
}
