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
callfile <- NULL
if(!endsWith(tfile, "test.py"))
  {
	callfile <- basename(tfile)
	bonus <- grepl("bonus", readLines(tfile,n=1), ignore.case=TRUE)
	tfile <- sub("s[kc]ript[0-9]*\\.py$","test.py",tfile)
	if(bonus) tfile <- sub("test.py", "testbonus.py", tfile)
  }

message("-- running py_local_score on ", tfile,
				if(!is.null(callfile)) paste0(", called from ", callfile))

berryFunctions::checkFile(tfile)
if(!(endsWith(tfile,"test.py")|endsWith(tfile,"testbonus.py")))
	stop("tfile must end in *test.py, but does not. ", tfile)

# Save all changes (py_local_score is not for students anyways):
rstudioapi::documentSaveAll()

# Actually run tests:
pycmd <- "python3"
if(Sys.info()["nodename"] == "DESKTOP-4RIP043") pycmd <- "python"
pyfile <- paste("-B -m unittest", basename(tfile))
tr <- suppressWarnings(system2(pycmd, pyfile, stderr=TRUE))
# Format output:
tr1 <- tr[1]
tr <- tr[startsWith(tr, "FAIL:") | startsWith(tr, "AssertionError:")]
tr <- gsub("AssertionError:", ":", tr, fixed=TRUE)
tr <- gsub("FAIL: test", "\n", tr, fixed=TRUE)
tr <- gsub("p..test.Script", "", tr)
tr <- paste(tr, collapse="")
tr <- sub("\n","",tr)
tr <- gsub(sub("py$","",basename(tfile)),"",tr)
# browser()
message(tr, "\n", tr1)
return(invisible(NULL))
}
