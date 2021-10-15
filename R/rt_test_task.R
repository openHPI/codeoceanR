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
#'                 can be FALSE for custom messages, e.g in multiple choice tasks.
#'                 DEFAULT: TRUE
#' @param zero     Check for pre-assigned objects (to 0) with special message? DEFAULT: hasval
#' @param dim      Check dimension (length or nrow+ncol)? DEFAULT: hasval
#' @param correct  Custom value message for multiple choice tasks? DEFAULT: !hasval
#' @param noise    noise parameter in [rt_has_value]. DEFAULT: FALSE
#' @param solved   Task number that must be solved before other tests are run. DEFAULT: NULL
#' @param names    Test whether `object` has the same [names] as `value`? DEFAULT: FALSE
#'
rt_test_task <- function(
tnumber,
script=NULL,
object=NULL,
value=NULL,
...,
zero=hasval,
hasval=TRUE,
dim=hasval,
correct=!hasval,
noise=FALSE,
solved=NULL,
names=FALSE
)
{
n <- deparse(substitute(object))
if(!is.numeric(tnumber)) stop("tnumber must be numeric, not ", toString(class(tnumber)), ", for tnumber: ", tnumber)
rt_env(id=tnumber)

# Exit this function through return() right after the first rt_warn message

# solved ----
if(!is.null(solved))
	{
	if(!rt_test(rt_env()$success[solved], "Please first solve task ",solved,"."))
		return(rt_env(fail=tnumber))
  }

# script ----
if(!is.null(script) && !rt_script_runs(script)) return(rt_env(fail=tnumber))

# object ----
if(n!="NULL" && !exists(n, envir=parent.frame()))
  {
  rt_warn("Create the object '",n,"'. It does not yet exist.")
	return(rt_env(fail=tnumber))
  }

# zero ----
if(zero && identical(object, 0))
  {
  rt_warn("Replace the 0 for '",n,"' with code to compute the solution.")
	return(rt_env(fail=tnumber))
  }

if(!is.null(value))
{
# class ----
class <- class(value)[1]
if(!inherits(object, class))
	{
  rt_warn("'", n,"' must be '", class, "', not of class '", toString(class(object)), "'.")
	return(rt_env(fail=tnumber))
  }

# length or nrows + ncols----
if(dim)
if(is.null(dim(value))) # vector, list
{
if(length(object)!=length(value))
  {
  rt_warn(n," must have length ", length(value),  ", not ", length(object), ".")
	return(rt_env(fail=tnumber))
  }
} else # dataframe, matrix, array
{
if(nrow(object)!=nrow(value))
  {
  rt_warn(n," must have ", nrow(value), " rows, not ", nrow(object), ".")
	return(rt_env(fail=tnumber))
  }
if(ncol(object)!=ncol(value))
  {
  rt_warn(n," must have ", ncol(value), " columns, not ", ncol(object), ".")
	return(rt_env(fail=tnumber))
  }
}

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
if(correct && !identical(sort(object),sort(value)))
	{
	rt_warn("The correct answer for '",n,"' is not ", toString(object), ".")
	return(rt_env(fail=tnumber))
  }

} # end !null(value)

# further tests ----
for(i in seq_len(...length())  )
   if(!...elt(i)) return(rt_env(fail=tnumber))

# pass ----
# set rt_test_env$success[tnumber] to TRUE if all tests passed:
rt_env(pass=tnumber)
}
