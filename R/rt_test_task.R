#' @title Wrapper for test script for an individual task within an exercise
#' @details NULL arguments mean that their respective tests are not performed.\cr
#'          Tests are performed in an order for increasingly specific
#'          messages that do not give away the solution too early.\cr
#'          Arguments after \dots need to be specified by full name.
#'          `tnumber` must always be given, `script`, `object` and `value`
#'          must be given (can be NULL) so later custom tests are not matched to them.
#' @return TRUE or FALSE, indicating whether all tests passed
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @seealso [rt_test_exercise], tests.R in the [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param tnumber  Number of task. Must be numeric, as it will be used for pass/fail in [rt_env].
#' @param script   Exercise script content that needs to be non-empty.
#'                 This is the only test that will not generate an [rt_warn] message,
#'                 as [rt_run_script] or [rt_select_script_section] already do.
#'                 DEFAULT: NULL
#' @param object   Object that needs to be created in student script.
#'                 Regular object name, not quoted. DEFAULT: NULL
#' @param value    Intended value. Existence, class and dimensions are checked first,
#'                 then [rt_has_value] is called if hasval=TRUE. DEFAULT: NULL
#' @param \dots    Further tests, comma-separated, can be given at the end.
#'                 Need to yield TRUE/FALSE and take care of [rt_warn] individually.
#'                 The `rt_*` test functions are designed for just that :).
#' @param hasval   After checks for existence, class and dimension, run [rt_has_value]?
#'                 Can be FALSE for custom messages, e.g in multiple choice tasks.
#'                 Will be FALSE if `value` is a function.
#'                 DEFAULT: TRUE
#' @param zero     Check for pre-assigned objects (to 0) with special message? DEFAULT: hasval
#' @param class    Class(es) that are acceptable. Test is passed if any of the classes matches.
#'                 Test is skipped if `class="any"`. DEFAULT: NULL (class of `value`)
#' @param dim      Check dimension (length or nrow+ncol) with [rt_has_dim]?
#'                 Will be FALSE if `value` is a function. DEFAULT: hasval
#' @param correct  Custom value message for multiple choice tasks?
#'                 Will be FALSE if `value` is a function. DEFAULT: !hasval
#' @param noise    noise parameter in [rt_has_value]. DEFAULT: FALSE
#' @param solved   Task number that must be solved before other tests are run. DEFAULT: NULL
#' @param inputs   List or vector with (named) charstrings with code to be called
#'                 if `object` and `value` are functions.
#'                 Will be called with `eval(str2lang(paste0("object(",input_i,")")))`.
#'                 Object names within rt_test_exercise are not available within rt_test_task.
#'                 Use a named object for a custom object name in rt_warn messages.
#'                 For single-argument functions, numerical input works fine, too.
#'                 These are the only tests run AFTER ...-tests are run.
#'                 DEFAULT: NULL
#' @param names    Test whether `object` has the same [names] as `value`? DEFAULT: FALSE
#'
rt_test_task <- function(
tnumber,
script=NULL,
object=NULL,
value=NULL,
...,
zero=hasval,
class=NULL,
hasval=TRUE,
dim=hasval,
correct=!hasval,
noise=FALSE,
solved=NULL,
inputs=NULL,
names=FALSE
)
{
n <- deparse(substitute(object))
if(!is.numeric(tnumber)) stop("tnumber must be numeric, not ", toString(class(tnumber)), ", for tnumber: ", tnumber)
rt_env(id=tnumber)

# Exit this function through return() right after the first rt_warn message

# script ----
if(!is.null(script) && !rt_script_runs(script)) return(rt_env(fail=tnumber))

# solved ----
if(!is.null(solved))
	{
	if(!rt_test(rt_env()$success[solved], "Please first solve task ",solved,"."))
		return(rt_env(fail=tnumber))
  }

# object ----
if(n!="NULL" && !exists(n, envir=parent.frame()))
  {
  rt_warn("Create the object '",n,"'. It does not yet exist.")
	return(rt_env(fail=tnumber))
  }

# zero ----
if(zero && identical(object, 0))
  {
  rt_warn("Replace the 0 for '",n,"' with code with the solution.")
	return(rt_env(fail=tnumber))
  }

if(!is.null(value))
{
# class ----
if(is.null(class)) class <- class(value)
if(!identical(class, "any") && !any(class(object) %in% class))
	{
  rt_warn("'", n,"' must be ", paste(class, collapse=" or "), ", not of class '", toString(class(object)), "'.")
	return(rt_env(fail=tnumber))
  }

if(is.function(value))
  {
	dim <- FALSE
  hasval <- FALSE
  correct <- FALSE
  }

# dim ----
if(dim && !rt_has_dim(object, value, name=n)) return(rt_env(fail=tnumber))

# names ----
if(names)
  {
  nv <- names(value)
  if(!all(nv %in% base::names(object))){
  rt_warn(n, " must have the name", if(length(nv)>1) "s:", " ", toString(nv), ".")
	return(rt_env(fail=tnumber))
  }}

# value ----
if(hasval && !rt_has_value(object, value, name=n, noise=noise))
	return(rt_env(fail=tnumber))
if(correct && !isTRUE(all.equal(sort(object),sort(value))))
	{
	rt_warn("The correct answer for '",n,"' is not ", toString(object), ".")
	return(rt_env(fail=tnumber))
  }

} # end !null(value)

# further tests ----
for(i in seq_len(...length())  )
   if(!...elt(i)) return(rt_env(fail=tnumber))

# function inputs ----
if(is.function(value))
{
if(zero && try(object(), silent=TRUE)==0)
  {
  rt_warn("'",n,"()' should not return 0.")
  return(rt_env(fail=tnumber))
  }
if(!is.null(inputs))
for(i in seq_along(inputs))
  {
	vec <- function(x) if(length(x)==1) x else paste0("c(",toString(x),")")
  uc <- paste0("object(",vec(inputs[[i]]),")") # user call
  cc <- paste0("value(",vec(inputs[[i]]),")") # correct call
  pc <- names(inputs)[i] ; if(is.null(pc)||pc=="") pc <- vec(inputs[[i]])
  pc <- paste0(n,"(",pc,")") # print call
	res    <- try(eval(str2lang(uc)), silent=TRUE)
	target <- try(eval(str2lang(cc)), silent=TRUE)
	if(inherits(res, "try-error"))
	  {
		res <- sub("^Error in .*?:", "", res)
		res <- gsub("\n", "", res)
    rt_warn("'",pc,"' should not yield Error: ",res)
    return(rt_env(fail=tnumber))
    }
	if(!rt_has_dim(res, target, name=pc)) return(rt_env(fail=tnumber))
	if(!rt_has_value(res, target, name=pc)) return(rt_env(fail=tnumber))
	} # end for loop
}
# pass ----
# set rt_test_env$success[tnumber] to TRUE if all tests passed:
rt_env(pass=tnumber)
}
