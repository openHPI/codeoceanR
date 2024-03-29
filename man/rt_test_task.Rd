% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_test_task.R
\name{rt_test_task}
\alias{rt_test_task}
\title{Wrapper for test script for an individual task within an exercise}
\usage{
rt_test_task(
  tnumber,
  object = NULL,
  value = NULL,
  ...,
  solved = NULL,
  correct = FALSE,
  igncase = FALSE,
  zero = TRUE,
  class = NULL,
  intnum = TRUE,
  dim = TRUE,
  funname = FALSE,
  names = TRUE,
  hasval = TRUE,
  stepwise = NULL,
  stepnames = FALSE,
  section = NULL,
  script = NULL,
  solcode = NULL,
  nameonly = FALSE,
  alt = NULL,
  opt = NULL,
  ignAssign = FALSE,
  inputs = NULL,
  export = NULL
)
}
\arguments{
\item{tnumber}{Number of task. If numeric, used for pass/fail in \link{rt_env}.}

\item{object}{Object that needs to be created in student script.
Regular object name, not quoted. First checked for existence.
If \code{value} is given, checked with \code{correct} and \code{zero},
then passed to \link{rt_test_object}.
If a function, it can be tested with \code{inputs}. DEFAULT: NULL}

\item{value}{Intended value. DEFAULT: NULL}

\item{\dots}{Further tests, comma-separated, can be given at the end.
Need to yield TRUE/FALSE and take care of \link{rt_warn} individually.
The \verb{rt_*} test functions are designed for just that :).}

\item{solved}{Task number that must be solved before any other tests are run.
DEFAULT: NULL}

\item{correct}{Custom value message for multiple choice tasks?
No further tests (except ...-tests) are run if correct=TRUE.
Do not set to TRUE for objects that cannot be sorted. DEFAULT: FALSE}

\item{igncase}{If \code{correct=TRUE} and \code{object} is a string,
ignore upper/lowercase and spaces? DEFAULT: FALSE}

\item{zero}{Check for pre-assigned objects (to 0) with special message?
Only needs to be FALSE if the intended value is 0. DEFAULT: TRUE}

\item{class}{Class(es) that are acceptable. Test is passed if any of the classes matches.
Test is skipped if \code{class="any"}.
Run through \link{rt_has_class}. DEFAULT: NULL (class of \code{value})}

\item{intnum}{Should numeric and integer class be interchangable? DEFAULT: TRUE}

\item{dim}{Check dimension (length or nrow+ncol)?
If FALSE, \code{names}, column classes and \code{hasval} are also not checked.
DEFAULT: TRUE}

\item{funname}{Check for function name as character string? Can have brackets.
DEFAULT: FALSE}

\item{names}{Check names (or rownames/colnames)?
If FALSE, column classes in data.frames are not checked!
DEFAULT: TRUE}

\item{hasval}{Run \link{rt_has_value}? DEFAULT: TRUE}

\item{stepwise}{stepwise parameter in \link{rt_has_value}. TRUE for arrays. DEFAULT: NULL}

\item{stepnames}{stepwise parameter for names check. DEFAULT: NULL}

\item{section}{Section number to be read with \link{rt_script_section} into \code{code}
which is then available in ... tests.
For manual evaluation, use \code{\link{eval}}\code{(}\code{\link{str2expression}}\verb{(code))},
not \code{\link{str2lang}}, in case students use line breaks.
\code{section} DEFAULT: NULL}

\item{script}{Exercise script content from \link{rt_run_script}.
Passed to \link{rt_script_section}. DEFAULT: NULL}

\item{solcode}{Solution code (charstring) to be checked with \link{rt_has_args}. DEFAULT: NULL}

\item{nameonly}{Literal checks? Passed to \link{rt_has_args}. DEFAULT: FALSE}

\item{alt}{List of alternative arguments. Passed to \link{rt_has_args}. DEFAULT: NULL}

\item{opt}{Vector of optional arguments. Passed to \link{rt_has_args}. DEFAULT: NULL}

\item{ignAssign}{Remove assignment part from code? Passed to \link{rt_has_args}. DEFAULT: FALSE}

\item{inputs}{List or vector with (named) charstrings with code to be called
if \code{object} and \code{value} are functions.
Will be called with \code{eval(str2lang(paste0("object(",input_i,")")))}.
Object names within rt_test_exercise are not available within rt_test_task,
unless listed in \code{export}.
For single-argument functions, numerical input works fine, too.
DEFAULT: NULL}

\item{export}{Character vector with object names to be assigned into evaluation environment.
DEFAULT: NULL}
}
\value{
TRUE or FALSE, indicating whether all tests passed. Also changes \link{rt_env} status.
}
\description{
Wrapper for test script for an individual task within an exercise
}
\details{
Tests are performed in an order for increasingly specific
messages that do not give away the solution too early.\cr
Arguments after \dots need to be specified by full name.
\code{tnumber} must always be given, \code{object} and \code{value}
must be given (can be NULL) so later custom tests are not matched to them.
}
\seealso{
\link{rt_test_exercise}, tests.R in the \href{https://github.com/openHPI/codeoceanR/tree/main/inst/extdata}{exercise example} on github
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de}, Oct 2021
}
