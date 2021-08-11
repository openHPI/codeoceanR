#' @title upload exercise to codeOcean
#' @description upload complete exercise, message CO Score results
#' @return [httr::content()] output of response to http request, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_local_score] for teachers, [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords test
#' @importFrom berryFunctions l2df
#' @importFrom httr POST content_type stop_for_status content
#' @importFrom rjson toJSON
#' @importFrom utils browseURL
#' @export
#'
#' @param dir Path to (processed) exercise folder.
#'            Must contain ".co" and all the "script_n.R" files
#'            referenced there, with the changes by the student, saved.
#'            DEFAULT: "."
#' @param submit Submit grade to openHPI? To be called from [rt_submit()].
#'
rt_score <- function(dir=".", submit=FALSE)
{
# Avoid recursive posting, so we can have rt_score() in the exercise script:
if(!interactive()) return(NULL)

# Check directory and file
dir <- berryFunctions::normalizePathCP(dir)
berryFunctions::checkFile(dir)
cofile <- paste0(dir, "/.co")
# run rt_local_score in exercise development folder with *tests.R:
if(!file.exists(cofile) && length(dir(dir, pattern=".*tests\\.R"))>0 )
	{
	message(".co file does not exist, running rt_local_score()")
	return(rt_local_score())
  }
# check if dir is an exercise directory
if(!file.exists(cofile)) stop("You're not in a CodeOcean exercise directory. ",
					"The file '.co' does not exist. \nYou're at '", dir, "'\n",
					"Make sure you have run rt_create_task() and the 'zz_exercise*.Rproj' file has been opened.\n",
					"In Rstudio on the top right, the R Project symbol should show the exercise name.")

# Warn if files are changed but not saved:
rt_check_for_unsaved_files(dir, warnonly=TRUE)

# get CO token + url + file IDs
co <- readLines(cofile, warn=FALSE)
co_token <- co[1]
co_url   <- co[2]
if(submit) co_url <- sub("evaluate", "submit", co_url)

# Get all file contents:
co_files <- co[-(1:2)]
if(length(co_files)<1) stop("No filenames found to be submitted in file '.co'")
co_files <- berryFunctions::l2df(strsplit(co_files, "="))
colnames(co_files) <- c("name", "id")
co_files$name <-  paste0(dir, "/", co_files$name)
berryFunctions::checkFile(co_files$name)
#
get_escaped_file_content <- function(fn)
  {
  d <- paste(readLines(fn, warn=FALSE), collapse="\n")
  d <- rjson::toJSON(d)
  d <- gsub("^\"", "", d) # remove leading + trailing quotation marks
  d <- gsub("\"$", "", d)
  d
  }
fileattr <- sapply(1:nrow(co_files), function(i) paste0('"',i-1,'": {"file_id": ',co_files$id[i],
							',"content": "',get_escaped_file_content(co_files$name[i]),'"}'))
fileattr <- paste(fileattr, collapse=", ")

# put into http request body:
body <- paste0('{"remote_evaluation": {"validation_token": "',co_token,
							 '","files_attributes": {',fileattr,'}}}')

# Post to CodeOcean:
r <- httr::POST(url=co_url, body=body, httr::content_type("application/json"))
erm <- httr::http_condition(r, "error")$message
if(grepl("Timeout was reached", erm)) # default timeout after 10 secs
	warning("You might be connected through a VPN. Try again without a proxy. Alternatively, the following might help:\n",
	'httr::set_config(httr::use_proxy(url="your.proxy.ip", port="port", username="user",password="pw"))',
	call.=FALSE)
if(submit) return(r)
httr::stop_for_status(r) # if any, pass http errors to R

# Output:
out <- httr::content(r, "parsed", "application/json")[[1]]
mout <- gsub("Rscript tests.R\n", "", out$stdout, fixed=TRUE)
mout <- gsub("AssertionError: ", "- ", mout, fixed=TRUE)
mout <- gsub("\n$", "", mout)
mout <- paste0(mout, ", score: ", round(out$score*100), "%")
message(mout) # print messages + score from codeOcean
return(invisible(out))
}
