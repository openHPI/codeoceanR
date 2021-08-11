#' @title Format exercise tasks for database
#' @description Take an entire exercise (`script_1.R`, `script_1.R`, `...`, `tests.R`)
#'              and format them to be added to the task database at
#'              <https://docs.google.com/spreadsheets/d/1ggSOYQ_veXgPmvA8cHCLnASkLUvc6-3t6UhJWPRPVoQ>.\cr
#'              Basically, the opposite of [rt_setup_exercise()].
#' @return location of tempfile with data.frame with tasks and reduced tests.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2020
#' @export
#' @examples
#' if(FALSE) # suppress automated runs
#' rt_exercise_to_db("C:/Users/berry/Desktop/CORQUIZ")
#'
#' @param dir Directory with `script_1.R`, `script_1.R`, `...`, `tests.R`. DEFAULT: "exercise99"
#' @param ntasks Number of tasks to obtain. DEFAULT: 10
#'
rt_exercise_to_db <- function(
dir="exercise99",
ntasks=10
)
{
dir <- berryFunctions::normalizePathCP(dir)
berryFunctions::checkFile(dir)
fs <- dir(dir, pattern="script_.*.R", full.names=TRUE)
ft <- paste0(dir, "/tests.R")
if(length(fs)<1) stop("No 'script_*.R' files were found at dir: ", dir)
if(!file.exists(ft)) stop("No 'tests.R' file was found at dir: ", dir)
#
select_parts <- function(r, tag="# Task .* -----")
{
lapply(1:ntasks, function(i)
  {
  tbeg <- grep(gsub(".*",i,tag, fixed=TRUE), r, fixed=TRUE)
  if(length(tbeg)==0) return()
  tend <- grep(tag, r)
  tend <- tend[tend>tbeg]
  tend <- if(length(tend)==0)   length(r)   else   min(tend)-1
  out <- gsub("^# ", "", r[tbeg:tend])
  out
  })
}
# scripts
tasks <- sapply(fs, function(f)
	{
	r <- readLines(f)
	r <- r[substr(r,1,25)!="# Now continue in script_"]
	r <- r[r!=""]
  select_parts(r)
  })
tasks <- tasks[!sapply(tasks, is.null)]
tasks <- lapply(tasks, function(x) x[-1])
# tests
r <- readLines(ft)
r <- r[r!="" & r!="&&" & r!="if(" & r!=") npassed <- npassed + 1"]
r <- r[substr(r,1,15)!="rt_script_runs("]
beg <- which(r=="task_id <- 1 # 1 ------")
end <- which(r=="}, silent=TRUE)")
if(length(beg)!=1) stop("The row 'task_id <- 1 # 1 ------' must occur exactly once in tests.R.")
if(length(end)!=1) stop("The row '}, silent=TRUE)' must occur exactly once in tests.R.")
r <- r[beg:(end-1)]
tests <- select_parts(r, "task_id <- .* # .* ------")
tests <- lapply(tests, function(x) x[-1])
tfile <- tempfile(fileext=".xlsx")
openxlsx::write.xlsx(x=data.frame(sapply(tasks, paste, collapse="\n"),
																	sapply(tests, paste, collapse="\n")),
										 file=tfile, colNames=FALSE)
berryFunctions::openFile(tfile)
tfile
}
