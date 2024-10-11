#' @title locally score python exercise
#' @description Run python exercise test script without server access.
#'              Intended for teacher / trainer use only.
#'              Requires *test.py script to be present (hidden on CodeOcean and not downloaded in zip folder).
#' @return NULL
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Sept 2024
#' @keywords test
#' @importFrom berryFunctions checkFile normalizePathCP
#' @param tfile Name of *test.py file to be run.
#'              DEFAULT: NULL (will be obtained from currently open File in Rstudio)
#'
py_local_score <- function(tfile=NULL)
{
# Obtain test file for currently selected document:
if(is.null(tfile)) tfile <- rstudioapi::documentPath()
# tfile <- "C:/Dropbox/R/kurs/pymooc/aufgaben/sipl32_skript1.py"
# tfile <- "C:/Dropbox/R/kurs/py_exercises/p32script1.py"
callfile <- basename(tfile)
if(!endsWith(tfile, "test.py"))
  {
	bonus <- grepl("bonus", readLines(tfile,n=1), ignore.case=TRUE)
	tfile <- sub("s[kc]ript[0-9]*\\.py$","test.py",tfile)
	if(bonus) tfile <- sub("test.py", "testbonus.py", tfile)
  }

berryFunctions::checkFile(tfile)
if(!(endsWith(tfile,"test.py")|endsWith(tfile,"testbonus.py")))
	stop("tfile must end in *test.py, but does not. ", tfile)

# Save all changes (py_local_score is not for students anyways):
rstudioapi::documentSaveAll()
rstudioapi::executeCommand('activateConsole')

# Actually run tests:
pycmd <- "python3"
if(Sys.info()["nodename"] == "DESKTOP-4RIP043") pycmd <- "python"
pyfile <- paste("-B -m unittest", basename(tfile))
message(paste0("-- running   ", pycmd," ", pyfile,"   called from ", callfile))
tp <- suppressWarnings(system2(pycmd, pyfile, stderr=TRUE))
tr <- tp
# Format output:
tr1 <- tr[1]
if(length(tr)<20) # catch development issues
	{
	message(paste(tr, collapse="\n"))
	return(invisible(NULL))
  }
# browser()
tr <- tr[startsWith(tr, "FAIL:") | startsWith(tr, "AssertionError:")]
tr <- gsub("AssertionError:", ":", tr, fixed=TRUE)
tr <- gsub("FAIL: test", "\n", tr, fixed=TRUE)
tr <- gsub("p..test.Script", "", tr)
tr <- paste(tr, collapse="")
tr <- sub("\n","",tr)
tr <- gsub(sub("py$","",basename(tfile)),"",tr)
message(tr, "\n", tr1)
return(invisible(tp))
}
