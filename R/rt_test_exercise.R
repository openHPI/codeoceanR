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
oop <- options(warn=1) # display warnings immediately, to aid tracking them down
on.exit(options(oop), add=TRUE)
rt_test_env <- list2env(rt_env(id="na")) # initialize environment

# eval + substitute: https://stackoverflow.com/q/69570220/1587132
trytests <- rt_try(eval(substitute(expr))) # rt_test_task calls

#rt_env(id="t")
if(inherits(trytests, "try-error"))
  {
	if(rt_env()$lang=="de")
  rt_warn(" ----- Test fehlgeschlagen. Bitte melden, siehe unten. -----\n",
  				"Das interne Testskript hat versagt. Dies sollte nie passieren. Sorry!!!\n",
          "Um wieder Punkte zu erhalten, mache bitte die letzten \u00C4nderungen r\u00FCckg\u00E4ngig.\n",
          "Bitte poste diese Fehlermeldung im Forum (wenn schon gemeldet, bitte upvoten).'\n----------\n",
  				trytests, "\n----------")
	else
  rt_warn(" ----- Test failed. Please report, see below. -----\n",
  				"The internal test script failed. This should never happen. Sorry!!\n",
          "To get scores again, please revert the last thing(s) you did.\n",
          "Please post this message in the forum (if already reported, please upvote).\n----------\n", trytests, "\n----------")
	rt_env(fail=1:99) # reset to zero to avoid 100% score up to failed rt_test_task
  }
# For succesfull testing, write results in CodeOcean format:
cat(length(rt_test_env$success), "examples,",
    sum(rt_test_env$success, na.rm=TRUE), "passed\n")
}
