#' @title Wrapper for test script for an individual task within an exercise
#' @details Tests are performed in an order for increasingly specific
#'          messages that do not give away the solution too early.\cr
#'          Arguments after \dots need to be specified by full name.
#'          `tnumber` must always be given, `script`, `object` and `value`
#'          must be given (can be NULL) so later custom tests are not matched to them.
#' @return TRUE or FALSE, indicating whether all tests passed. Also changes [rt_env] status.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @seealso [rt_test_exercise], tests.R in the [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param tnumber  Number of task. Must be numeric, as it will be used for pass/fail in [rt_env].
#' @param script   Exercise script content from [rt_run_script] or [rt_select_script_section].
#'                 This is the only test that will not generate an [rt_warn] message,
#'                 as the previous functions already do. DEFAULT: NULL
#' @param object   Object that needs to be created in student script.
#'                 Regular object name, not quoted. First checked for existence.
#'                 If `value` is given, checked with `correct` and `zero`,
#'                 then passed to [rt_test_object]. DEFAULT: NULL
#' @param value    Intended value. DEFAULT: NULL
#' @param \dots    Further tests, comma-separated, can be given at the end.
#'                 Need to yield TRUE/FALSE and take care of [rt_warn] individually.
#'                 The `rt_*` test functions are designed for just that :).
#' @param solved   Task number that must be solved before any other tests are run
#'                 (except script test). DEFAULT: NULL
#' @param correct  Custom value message for multiple choice tasks?
#'                 No further tests (except ...-tests) are run if correct=TRUE.
#'                 Do not set to TRUE for objects that cannot be sorted. DEFAULT: FALSE
#' @param zero     Check for pre-assigned objects (to 0) with special message?
#'                 Only needs to be FALSE if the intended value is 0. DEFAULT: TRUE
#' @param class    Class(es) that are acceptable. Test is passed if any of the classes matches.
#'                 Test is skipped if `class="any"`.
#'                 Run through [rt_has_class]. DEFAULT: NULL (class of `value`)
#' @param intnum   Should numeric and integer class be interchangable? DEFAULT: TRUE
#' @param dim      Check dimension (length or nrow+ncol)?
#'                 If FALSE, `names`, column classes and `hasval` are also not checked.
#'                 DEFAULT: TRUE
#' @param names    Check names (or rownames/colnames)? DEFAULT: TRUE
#' @param hasval   Run [rt_has_value]? DEFAULT: TRUE
#' @param noise    noise parameter in [rt_has_value]. DEFAULT: FALSE
#' @param stepwise stepwise parameter in [rt_has_value]. DEFAULT: NULL
#' @param stepnames stepwise parameter for names check. DEFAULT: NULL
#' @param inputs   List or vector with (named) charstrings with code to be called
#'                 if `object` and `value` are functions.
#'                 Will be called with `eval(str2lang(paste0("object(",input_i,")")))`.
#'                 Object names within rt_test_exercise are not available within rt_test_task,
#'                 unless listed in `export`.
#'                 For single-argument functions, numerical input works fine, too.
#'                 These are the only tests run AFTER ...-tests are run.
#'                 DEFAULT: NULL
#' @param export   Character vector with object names to be assigned into evaluation environment.
#'                 DEFAULT: NULL
#'
rt_test_task <- function(
tnumber,
script=NULL,
object=NULL,
value=NULL,
...,
solved=NULL,
correct=FALSE,
zero=TRUE,
class=NULL,
intnum=TRUE,
dim=TRUE,
names=TRUE,
hasval=TRUE,
noise=FALSE,
stepwise=NULL,
stepnames=FALSE,
inputs=NULL,
export=NULL
)
{
n <- deparse(substitute(object))
if(!is.numeric(tnumber)) stop("tnumber must be numeric, not ", toString(class(tnumber)), ", for tnumber: ", tnumber)
rt_env(id=tnumber)

# Exit this function through return() right after the first rt_warn message

# script ----
if(!is.null(script) && (isFALSE(script) || identical(script,"FALSE")) )
	return(rt_env(fail=tnumber))
	# identical needed in case things like gsub("'", "\"", code) have been run:

# solved ----
if(!is.null(solved))
	if(!rt_test(rt_env()$success[solved], "Please first solve task ",solved,"."))
		return(rt_env(fail=tnumber))

# object created ----
if(n!="NULL" && !exists(n, envir=parent.frame()))
  {
  rt_warn("Create the object '",n,"'.")
	return(rt_env(fail=tnumber))
  }

# the rest (up to ...-tests) only if value is given:
if(!is.null(value))
{
# correct ----
if(correct && !is.function(value))
	{
	if(!is.atomic(object))
		{
	  rt_warn("'",n,"' must be atomic (a vector), not of class '", toString(class(object)), "'.")
	  return(rt_env(fail=tnumber))
	  }
	if(!isTRUE(all.equal(sort(object),sort(value))))
		{
	  rt_warn("The correct answer for '",n,"' is not ", toString(object), ".")
	  return(rt_env(fail=tnumber))
	  }
  }
if(correct){zero <- FALSE ; class <- "any" ; dim <- FALSE} # only run ...-tests after this

# zero ----
if(zero && identical(object, 0))
  {
  rt_warn("Replace the 0 for '",n,"' with code with the solution.")
	return(rt_env(fail=tnumber))
  }
if(zero && is.function(value) && try(object(), silent=TRUE)==0)
  {
  rt_warn("'",n,"()' should not return 0.")
  return(rt_env(fail=tnumber))
  }

# test_object ----
if(!rt_test_object(object, value, name=n, class=class, intnum=intnum, dim=dim, names=names,
	hasval=hasval, noise=noise, stepwise=stepwise, stepnames=stepnames)) return(rt_env(fail=tnumber))

} # end !null(value)

# further tests ----
for(i in seq_len(...length())  )
   if(!...elt(i)) return(rt_env(fail=tnumber))

# function inputs ----
if(is.function(value))
{
if(!is.null(inputs) && !is.null(export))
	{
	for(e in export) assign(e, dynGet(e)) # get from parent frame (not parent env)
  }
if(!is.null(inputs))
for(i in inputs)
  {
	vec <- function(x) if(length(x)==1) x else rt_vec(x)
  uc <- paste0("object(",vec(i),")") # user call
  cc <- paste0( "value(",vec(i),")") # correct call
  pc <- paste0(    n,"(",vec(i),")") # print call
	res    <- try(eval(str2lang(uc)), silent=TRUE)
	target <- try(eval(str2lang(cc)), silent=TRUE)
	if(inherits(res, "try-error"))
	  {
		res <- sub("^Error in .*?:", "", res)
		res <- gsub("\n", "", res)
    rt_warn("'",pc,"' should not yield Error: ",res)
    return(rt_env(fail=tnumber))
    }
  if(!rt_test_object(res, target, name=pc, class=class, intnum=intnum, dim=dim, names=names,
	  hasval=hasval, noise=noise, stepwise=stepwise, stepnames=stepnames)) return(rt_env(fail=tnumber))
	} # end for loop
}
# pass ----
# set rt_test_env$success[tnumber] to TRUE if all tests passed:
rt_env(pass=tnumber)
}
