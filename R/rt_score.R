#' @title score complete exercise
#' @description Run test script for entire exercise
#' @return Vector with number of total and passed tests, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso \code{\link{help}}, \code{graphics::\link[graphics]{plot}}
#' @keywords test
#' @importFrom berryFunctions checkFile
#' @export
#' @examples
#' # ToDo
#'
#' @param tfile Path to tests.R file. ToDo: automated detection. DEFAULT: "tests.R"
#'
rt_score <- function(tfile="tests.R")
{
berryFunctions::checkFile(tfile)
# ToDo: figure out if unsaved file changes can be detected
# ToDo: find better system for task_id updates. Tthis is a patchy mess
if(exists("task_id"))
	{
	warning("Removing object 'task_id' from globalenv workspace.")
	rm(task_id, envir=globalenv())
  }
source(tfile, local=TRUE) # to keep this local, rt_warn must search for 'task_id'
# Output:
return(invisible(c(ntests=ntests, npassed=npassed)))
}

# Suppress CRAN check note 'no visible binding for global variable':
if(getRversion() >= "2.15.1")  utils::globalVariables(c("ntests", "npassed"))
