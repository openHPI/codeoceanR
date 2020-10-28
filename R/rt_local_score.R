#' @title locally score complete exercise
#' @description Run test script for entire exercise without server access.
#'              Intended for teacher / trainer use only.
#'              Requires tests.R script to be present (hidden on CodeOcean and not downloaded in zip folder).
#' @return Vector with number of total and passed tests, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_score] for students. [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords test
#' @importFrom berryFunctions checkFile normalizePathCP
#' @export
#'
#' @param dir Path to exercise folder.
#'            Must contain "tests.R" and all the "script_n.R" files referenced there.
#'            DEFAULT: "."
#'
rt_local_score <- function(dir=".")
{
dir <- berryFunctions::normalizePathCP(dir)
berryFunctions::checkFile(dir)
tfile <- paste0(dir, "/tests.R")
berryFunctions::checkFile(tfile)
# ToDo: find better system for task_id updates. This is a patchy mess
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
