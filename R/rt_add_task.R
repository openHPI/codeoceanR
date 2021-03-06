#' @title Add task from database to script
#' @description Add a task from the database to `script_x.R`
#'              and the corresponding tests to `tests.R`.
#' @return taskfile
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_setup_exercise]
#'
#' @param tdb       Task database (data.frame with the columns ID, Task, Test)
#' @param id        ID to be selected from `tdb`, e.g. "Q10_2"
#' @param task_nr   Task number to be printed in `script_x.R`, DEFAULT: id
#' @param script_nr Script number to be used for this task. DEFAULT: 1
#' @param dir       Directory. DEFAULT: "./Quiz1"
#'
rt_add_task <- function(
tdb,
id,
task_nr=id,
script_nr=1,
dir="./Quiz1"
)
{

if(!id %in% tdb$ID) stop("ID is not in tdb: ", id)

# dir management:
dir <- berryFunctions::normalizePathCP(dir)
if(!dir.exists(dir)) dir.create(dir, recursive=TRUE)

# TASK ----
taskfile <- paste0(dir, "/script_",  script_nr, ".R")
# Write in finished task script:
if(script_nr>1 && !file.exists(taskfile))
	cat(paste0("\n\n# Now continue in script_",script_nr,".R\n"),
			file=paste0(dir,"/script_", script_nr-1,".R"), append=TRUE)
# start new script with instructions:
if(!file.exists(taskfile) || !any(grepl("codeoceanR::rt_score", readLines(taskfile), fixed=TRUE)) )
	cat(if(script_nr==1) "# Submission is due 16:00 CET. In the succeeding 15 minutes you still get 80% of the score.\n\n",
		"# To score, you can save + source the entire script (CTRL + SHIFT + S) to run",
      "\ncodeoceanR::rt_score()\n", file=taskfile, append=TRUE, sep="")

# text of exercise:
te <- tdb[id,'Task']
te <- gsub("\n", "\n# ", te, fixed=TRUE)
te <- gsub("tx_start", paste0("t",task_nr,"_start"), te, fixed=TRUE)
te <- gsub("tx_end"  , paste0("t",task_nr,"_end"  ), te, fixed=TRUE)

# Write task:
cat(paste0("\n\n# Task ",task_nr," -----\n\n# ", te, "\n\n"),
		file=taskfile, append=TRUE)

# TEST ----
testfile <- paste0(dir, "/tests.R")
# Copy template, add generation time:
if(!file.exists(testfile))
  {
  tlines <- readLines(system.file("extdata/template.R", package="codeoceanR"))
  tlines <- sub("Generated by rt_setup_exercise",
  							paste("Generated", Sys.time(), "by rt_setup_exercise"), tlines)
  writeLines(tlines, testfile)
  }

tlines <- readLines(testfile)
# run_script line:
rs <- paste0("script",script_nr," <- rt_run_script(\"script_",script_nr,".R\")")
if(!any(grepl(rs, tlines, fixed=TRUE))) tlines <- sub("# RUN SCRIPT", paste0(rs,"\n# RUN SCRIPT"), tlines)

# test code from database
tt <- tdb[id,"Test"]
if(is.na(tt) || tt=="---")
  {
  toprint <- paste0("task_id <- ",task_nr," # ",task_nr," ------\n\n# ", tt, "\n\n")
  tlines <- sub("# TEST TASK", paste0(toprint,"\n# TEST TASK"), tlines)
  writeLines(tlines, testfile)
  return(taskfile)
  }

# text of test:
tt <- strsplit(tt, "\n")[[1]]
tt <- gsub("\\<scriptx\\>", paste0("script",script_nr), tt)
tt <- gsub("\\<script_x\\>", paste0("script_",script_nr), tt)
tt <- gsub("\\<taskx\\>", task_nr, tt)

# Find pre + post test code:
pp <- substr(tt, 1,3)!="rt_"
l <- length(pp)
b <- min(which(!pp)) # begin actual tests
e <- max(which(!pp)) # end actual tests
pre  <- if(b>1) 1:(b-1) else 0
post <- if(e<l) (e+1):l else 0
mid  <- b:e

toprint <- paste0("task_id <- ",task_nr," # ",task_nr," ------\n",
paste(tt[pre], collapse="\n"),"\nif(\nrt_script_runs(script",script_nr,")\n&&\n",
paste(tt[mid], collapse="\n&&\n"),"\n) npassed <- npassed + 1\n",
paste(tt[post], collapse="\n"), "\n")
tlines <- sub("# TEST TASK", paste0(toprint,"\n# TEST TASK"), tlines, fixed=TRUE)
writeLines(tlines, testfile)
# Output:
return(taskfile)
}
