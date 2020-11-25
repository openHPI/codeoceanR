#' @title warn about errors
#' @description pass test messages to Rstudio / CodeOcean.
#' Relies on object 'task_id' to be found
#' @return Output of [warning()] in interactive mode (RStudio) or
#'                   [cat()] otherwise (CodeOcean)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords IO error
#' @seealso [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @export
#' @examples
#' task_id <- 5
#' rt_warn("This is a CodeOcean message!")
#'
#' @param \dots Message components passed to [warning()] or [cat()].
#'
rt_warn <- function(...){
  # get 'task_id' object from up to 3 levels up.
	# This enables rt_score to run in a temporary environment
	if(!exists("task_id"))             task_id <- try(get("task_id", envir=parent.frame(1)), silent=TRUE)
	if(inherits(task_id, "try-error")) task_id <- try(get("task_id", envir=parent.frame(2)), silent=TRUE)
	if(inherits(task_id, "try-error")) task_id <- try(get("task_id", envir=parent.frame(3)), silent=TRUE)
	if(inherits(task_id, "try-error")) task_id <- "_task_id_not_found_"
	# now actually warn:
	if(interactive())
              message("T", task_id, ": ", ...,       sep="") else
  cat("AssertionError: T", task_id, ": ", ..., "\n", sep="")
}

# Suppress CRAN check note 'no visible binding for global variable':
if(getRversion() >= "2.15.1")  utils::globalVariables(c("task_id"))
