#' @title Design complete exercise with tasks from database
#' @description Set up an entire exercise (`script_1.R`, `script_1.R`, `...`, `tests.R`).
#'              Uses the google spreadsheet as a database from which tasks can be selected by ID.
#'              To request access to the spreadsheet, go to
#'              <https://docs.google.com/spreadsheets/d/1ggSOYQ_veXgPmvA8cHCLnASkLUvc6-3t6UhJWPRPVoQ>.\cr
#'              The opposite direction is available with [rt_exercise_to_db()].
#' @return taskfile from last call to [rt_add_task]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @export
#' @importFrom utils tail
#' @examples
#' if(FALSE) # suppress automated runs
#' rt_setup_exercise(dir="C:/Users/berry/Desktop/CORQUIZ",
#' df=read.table(header=TRUE, text="
#' id      task  script
#' Q11_1   1     1
#' Q10_2   2     1
#' Q12_10  3     2
#' Q11_2   4     3
#' Q11_3   5     3"))
#'
#' if(FALSE)
#' unlink("C:/Users/berry/Desktop/CORQUIZ", recursive=TRUE, force=TRUE)
#'
#' if(FALSE)
#' rt_setup_exercise(df=data.frame(id="pack3", task=1, script=1),
#'                   dir="C:/Users/berry/Desktop/CORQUIZ")
#'
#' @param df  Data.frame with id, task, script. See examples.
#' @param dir Directory. May not yet exist to avoid overwriting. DEFAULT: "./Quiz1"
#'
rt_setup_exercise <- function(
df,
dir="./Quiz1"
)
{
dir <- berryFunctions::normalizePathCP(dir)
if(dir.exists(dir)) stop("dir already exists. Please choose a new location. path = ", dir)
dir.create(dir, recursive=TRUE)
# write .Rproj file
rprojfile <- paste0(dir, "/zz_development_Rquiz.Rproj")
rprojfile <- berryFunctions::normalizePathCP(rprojfile)
cat("Version: 1.0\n\nRestoreWorkspace: No\nSaveWorkspace: No\nEncoding: UTF-8", file=rprojfile)
#
# task database (tdb):
tdb <- googlesheets4::read_sheet("1ggSOYQ_veXgPmvA8cHCLnASkLUvc6-3t6UhJWPRPVoQ")
tdb <- as.data.frame(tdb)
rownames(tdb) <- tdb$ID
if(identical(df, "all")) df <- data.frame(id=tdb$ID, task=1:nrow(tdb), script=1)
if(requireNamespace("pbapply", quietly=TRUE)) lapply <- pbapply::pblapply
out <- lapply(1:nrow(df), function(i)
	rt_add_task(tdb=tdb, id=df$id[i], task_nr=df$task[i], script_nr=df$script[i], dir=dir))
out <- tail(unlist(out),1)
cat("\n\n\n# submit for grading ----\n\n# codeoceanR::rt_submit() # in the console (!),",
		"confirm you really want to submit\n\n", file=out, append=TRUE)
#
# put scripts to Rstudio opened files list:
rt_add_opened_files(dir(dir,pattern="script_"), dir=dir)
rt_add_opened_files("tests.R", dir=dir)
# try to open Rproject:
message("Opening ", rprojfile, "\nOpen manually if this fails.")
berryFunctions::openFile(rprojfile)
#
return(out)
}
