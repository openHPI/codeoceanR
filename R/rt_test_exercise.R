#' @title Wrapper for test script for complete exercise
#' @return NULL, writes to the console with [cat].
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @seealso [rt_test_task], tests.R in the [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords test
# @importFrom package fun1 fun2
#' @export
#' @examples
#' rt_test_exercise(rnorm(20))
#'
#' @param expr calls to [rt_test_task], in brackets
#'
rt_test_exercise <- function(expr)
{
trytests <- try(expr, silent=TRUE)
task_id <- "t"
if(inherits(trytests, "try-error"))
  rt_warn("The internal test script failed. This should never happen. Sorry!!\n",
          "To get scores again, please revert the last thing(s) you did.\n",
          "Please send Berry the logfile below through email or 'Request comments':\n", trytests)
# For succesfull testing, write results in CodeOcean format:
cat(length(taskenvironment$success), "tests,",
    sum(taskenvironment$success, na.rm=TRUE), "passed\n")
}



#' @title Wrapper for test script for an individual task within an exercise
#' @return TRUE or FALSE, indicating whterh all tests passed
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2021
#' @seealso [rt_test_exercise], tests.R in the [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#'
#' @param tnumber  Number of task.
#' @param \dots    Further tests, comma-separated
#' @param script   DEFAULT: NULL
#' @param object   DEFAULT: NULL
#' @param class    DEFAULT: NULL
#' @param length   DEFAULT: NULL
#' @param nrows    DEFAULT: NULL
#' @param ncols    DEFAULT: NULL
#' @param names    DEFAULT: NULL
#' @param value    DEFAULT: NULL
#' @param noise    DEFAULT: FALSE
#'
rt_test_task <- function(
tnumber,
...,
script=NULL,
object=NULL,
class=NULL,
length=NULL,
nrows=NULL,
ncols=NULL,
names=NULL,
value=NULL,
noise=FALSE
)
{
taskenvironment$task_id <- tnumber
if(!is.null(script)) if(!rt_script_runs(script)      ) return(rt_success(tnumber, v=FALSE))
if(!is.null(object)) if(!rt_exists(object)           ) return(rt_success(tnumber, v=FALSE))
if(!is.null(class) ) if(!rt_has_class(object, class) ) return(rt_success(tnumber, v=FALSE))
if(!is.null(length)) if(!rt_has_length(object,length)) return(rt_success(tnumber, v=FALSE))
if(!is.null(nrows) ) if(!rt_has_nrows(object, nrows) ) return(rt_success(tnumber, v=FALSE))
if(!is.null(ncols) ) if(!rt_has_ncols(object, ncols) ) return(rt_success(tnumber, v=FALSE))
if(!is.null(names) ) if(!rt_has_names(object, names) ) return(rt_success(tnumber, v=FALSE))
if(!is.null(value) ) if(!rt_has_value(object, value, noise=noise) ) return(rt_success(tnumber, v=FALSE))
for(i in seq_len(...length())  )
   if(!...elt(i)) return(rt_success(tnumber, v=FALSE))
return(rt_success(tnumber, v=TRUE)) # set to TRUE if all tests passed :)
}
