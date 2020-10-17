#' @title warn about errors
#' @description pass test messages to Rstudio / CodeOcean.
#' Relies on object 'task' to be found, e.g. in globalEnv.
#' @return Output of [warning()] in interactive mode (RStudio) or
#'                   [cat()] otherwise (CodeOcean)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @keywords IO error
# @importFrom package fun1 fun2
#' @export
#' @examples
#' task <- 5
#' rt_warn("This is a CodeOcean message!")
#'
#' @param \dots Message components passed to [warning()] or [cat()].
#'
rt_warn <- function(...) if(interactive())
  {            warning("T", task, ": ", ..., "\n", sep="", call.=FALSE)} else
  {cat("AssertionError: T", task, ": ", ..., "\n", sep="")             }

# Suppress CRAN check note 'no visible binding for global variable':
if(getRversion() >= "2.15.1")  utils::globalVariables(c("task"))
