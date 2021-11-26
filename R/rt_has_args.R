#' @title Check code chunk with function call for correct arguments, e.g. in plotting tasks.
#' @description Check student code for certain argument values. Matching is done internally.
#' @details Used to be covered by [rt_has_argument], which might be deprecated.
#' @return TRUE / FALSE
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2021
#' @seealso [str2lang], [match.call]
#' @export
#' @examples
#' rt_has_args("plot(1:5, lwd=2)", plot(1:5, lwd=3), 7)
#' # Tx: code section t7: argument 'lwd' should be '3', not '2'.
#'
#' # 'data' arguments are attached internally, hence this works:
#' rt_has_args("plot(rate~conc,data=Puromycin,col=state)",
#'              plot(rate~conc,data=Puromycin,col=state), 7)
#'
#' rt_has_args("plot(1:5, lwd=2)", plot(1:5, lwd=3), 7, alt=list(lwd=2:4)) # TRUE
#'
#' rt_has_args("plot(c(1,2,3,4,5))", plot(1:5), 7               ) # TRUE
#' rt_has_args("plot(c(1,2,3,4,5))", plot(1:5), 7, nameonly=TRUE) # FALSE:
#' # Tx: code section t7: argument 'x' should be '1:5', not 'c(1, 2, 3, 4, 5)'.
#'
#' # See more in unit tests at "codeoceanR/tests/testthat/argtests.R"
#' @param code     Charstring with user (student) code
#' @param expr     Expression with desired outcome.
#' @param snumber  Section number for messaging.
#' @param nameonly Check for e.g. 1:5 exactly, disallowing c(1,2,3,4,5).
#'                 DEFAULT: FALSE
#' @param stepwise Passed to [rt_test_object]. DEFAULT: NULL
#' @param alt      list of alternately accepted inputs.
#'                 Use list(argname="anyval", argname="optional")
#'                 to skip value / presence test, respectively. DEFAULT: NULL
#'
rt_has_args <- function(
code,
expr,
snumber,
nameonly=FALSE,
stepwise=NULL,
alt=NULL
)
{
cs <- paste0("code section t",snumber)

# Language object from character string:
if(length(code)>1) code <- paste(code, collapse="\n")
code2 <- try(str2lang(code), silent=TRUE)
if(inherits(code2,"try-error"))
	return(rt_warn("str2lang for ",cs," produced error: ",
								 attr(code2,"condition")$message))

# User function
u_fun <- code2[[1]]
# Intended solution:
# if not coming from rt_test_task (where substitute has already been called):
if(!any(grepl("rt_test_task", sys.calls()))) expr <- substitute(expr)
i_fun <- expr[[1]]
if(i_fun != u_fun) return(rt_warn(cs," should contain the function '",i_fun,"', not '",u_fun,"'."))

# user and intended arguments
u_arg <- as.list(match.call(eval(u_fun), code2))[-1] # all args, except function name
i_arg <- as.list(match.call(eval(i_fun), expr))[-1]

if(is.null(names(i_arg))) return(rt_warn(cs, ": argument names cannot be matched in trainer code. Please report this."))
if(is.null(names(u_arg))) return(rt_warn("Arguments in '",u_fun,"' must be named explicitely in ", cs,"."))

# Duplicated arguments:
dup <- duplicated(names(u_arg))
if(any(dup)) return(rt_warn(cs," should not contain the argument '",
											 toString(unique(names(u_arg)[dup])),"' more than once."))

# copy objects from student-script here so eval(x) will find them:
for(n in ls(1)) assign(n, get(n,1))
attach(eval(u_arg$data), warn.conflicts=FALSE)

# Evaluate/deparse arguments:
# eval environment in formula call?
argfun <- function(x) if(is.character(x)) dQuote(x, '"') else
	                    if(nameonly       ) deparse(x) else
	                    	                  eval(x)
u_arg <- lapply(u_arg, argfun)
i_arg <- lapply(i_arg, argfun)

# Presence and value of arguments:
for(n in names(i_arg))
  {
  if(!isTRUE(alt[[n]]=="optional"))
  if(!n %in% names(u_arg)) return(rt_warn(cs," should contain the argument '",n,"'."))
	inalt <- try(u_arg[[n]] %in% alt[[n]], silent=TRUE)
	if(isTRUE(inalt) || isTRUE(alt[[n]]=="anyval") || suppressWarnings(isTRUE(all(inalt)))) next
	if(!rt_test_object(u_arg[[n]], i_arg[[n]], name=paste0(cs,": argument '",n,"'"),
										 qmark=FALSE, stepwise=stepwise)) return(FALSE)
  }

# If all arguments are correct:
return(TRUE)
}
