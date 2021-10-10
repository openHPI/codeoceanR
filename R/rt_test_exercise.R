#' @title Wrapper for test script for complete exercise
#' @return NULL, writes to the console with [cat].
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @seealso [rt_test_task], _tests.R in the [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords test
#' @export
#' @param expr calls to [rt_test_task], in curly brackets
#'
rt_test_exercise <- function(expr)
{
rt_test_env <- new.env()
rt_test_env$success <- vector()
rt_env(id="na")
trytests <- try(expr, silent=TRUE) # rt_test_task calls
#rt_env(id="t")
if(inherits(trytests, "try-error"))
  {
  rt_warn("The internal test script failed. This should never happen. Sorry!!\n",
          "To get scores again, please revert the last thing(s) you did.\n",
          "Please send Berry the logfile below through email or 'Request comments':\n", trytests)
	rt_env(fail=1:99) # reset to zero to avoid 100% score up to failed rt_test_task
  }
# For succesfull testing, write results in CodeOcean format:
cat(length(rt_test_env$success), "tests,",
    sum(rt_test_env$success, na.rm=TRUE), "passed\n")
}



#' @title Wrapper for test script for an individual task within an exercise
#' @details NULL arguments mean that their respective tests are not performed.\cr
#'          Tests are performed in order of arguments for increasingly specific
#'          messages that do not give away the solution too early.\cr
#'          \dots must be the first argument so custom tests are not matched to them.
#'          (except for `tnumber`, the other tests are optional).
#'          Hence all other arguments need to be specified by full name.\cr
#'          Common test functions may get their own arguments
#'          (like [rt_has_value] through `value`) or be incorporated completely,
#'          like the formerly separate `rt_has_class`.
#' @return TRUE or FALSE, indicating whether all tests passed
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @seealso [rt_test_exercise], tests.R in the [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param tnumber  Number of task. Must be numeric, as it will be used for pass/fail in [rt_env].
#' @param \dots    Further tests, comma-separated, can be given at the end.
#'                 Need to yield TRUE/FALSE and take care of [rt_warn] individually.
#'                 The `rt_*` test functions are designed for just that :).
#' @param script   Exercise script content that needs to be non-empty.
#'                 This is the only test that will not generate an [rt_warn] message,
#'                 as [rt_run_script] or [rt_select_script_section] already do.
#'                 DEFAULT: NULL
#' @param object   Object that needs to be created in student script.
#'                 Regular object name, not quoted. DEFAULT: NULL
#' @param zero     Check for pre-assigned objects (to 0) with special message? DEFAULT: TRUE
#' @param class    Required object [class]. DEFAULT: NULL
#' @param length   Required object [length]. DEFAULT: NULL
#' @param nrows    Required [nrow] of object. DEFAULT: NULL
#' @param ncols    Required [ncol] of object. DEFAULT: NULL
#' @param names    Required object [names]. DEFAULT: NULL
#' @param value    Intended value, see [rt_has_value]. DEFAULT: NULL
#' @param noise    noise parameter in [rt_has_value], if `value` != NULL. DEFAULT: FALSE
#'
rt_test_task <- function(
tnumber,
...,
script=NULL,
object=NULL,
zero=TRUE,
class=NULL,
length=NULL,
nrows=NULL,
ncols=NULL,
names=NULL,
value=NULL,
noise=FALSE
)
{
n <- deparse(substitute(object))
if(!is.numeric(tnumber)) stop("tnumber must be numeric, not ", toString(class(tnumber)), ", for tnumber: ", tnumber)
rt_env(id=tnumber)

# Exit this function through return() right after the first rt_warn message

# script ----
if(!is.null(script) && !rt_script_runs(script)) return(rt_env(fail=tnumber))

# object ----
if(n!="NULL" && !base::exists(n))
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

# class ----
if(!is.null(class) && !inherits(object, class))
	{
  rt_warn(n," must be '", class, "', not of class '", toString(class(object)), "'.")
	return(rt_env(fail=tnumber))
  }

# length ----
if(!is.null(length) && length(object)!=length)
  {
  rt_warn(n," must have length ", length,  ", not ", length(object), ".")
	return(rt_env(fail=tnumber))
  }

# nrows ----
if(!is.null(nrows) && nrow(object)!=nrows)
  {
  rt_warn(n," must have ", nrows, " rows, not ", nrow(object), ".")
	return(rt_env(fail=tnumber))
  }

# ncols ----
if(!is.null(ncols) && ncol(object)!=ncols)
  {
  rt_warn(n," must have ", ncols, " columns, not ", ncol(object), ".")
	return(rt_env(fail=tnumber))
  }

# names ----
if(!is.null(names) && !all(names %in% base::names(object)))
  {
  rt_warn(n, " must have the name", if(length(names)>1) "s:", " ", toString(n), ".")
	return(rt_env(fail=tnumber))
  }

# value ----
if(!is.null(value) && !rt_has_value(object, value, name=n, noise=noise))
	return(rt_env(fail=tnumber))

# further tests ----
for(i in seq_len(...length())  )
   if(!...elt(i)) return(rt_env(fail=tnumber))

# pass ----
# set rt_test_env$success[tnumber] to TRUE if all tests passed:
rt_env(pass=tnumber)
}
