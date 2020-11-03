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
#' @param final Print instructions (+ open URL, if available) for final submission?
#'            DEFAULT: FALSE
#'
rt_score <- function(dir=".", final=FALSE)
{
# Avoid recursive posting in case students leave rt_score() in the exercise script:
if(!interactive()) return(NULL)

# Check directory and file
dir <- berryFunctions::normalizePathCP(dir)
berryFunctions::checkFile(dir)
cofile <- paste0(dir, "/.co")
berryFunctions::checkFile(cofile)

# Stop if files are changed but not saved:
rt_check_for_unsaved_files(dir)

# get CO token + url + file IDs
co <- readLines(cofile)
co_token <- co[1]
co_url   <- co[2]

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
  d <- paste(readLines(fn), collapse="\n")
  d <- rjson::toJSON(d)
  d <- gsub("^\"", "", d) # remove leading + trailing quotation marks
  d <- gsub("\"$", "", d)
  d
  }
fileattr <- sapply(1:nrow(co_files), function(i) paste0('{"file_id": ',co_files$id[i],
							',"content": "',get_escaped_file_content(co_files$name[i]),'"}'))
fileattr <- paste(fileattr, collapse=", ")

# put into http request body:
body <- paste0('{"remote_evaluation": {"validation_token": "',co_token,
							 '","files_attributes": [',fileattr,']}}')

# Post to CodeOcean:
r <- httr::POST(url=co_url, body=body, httr::content_type("application/json"))
httr::stop_for_status(r) # if any, pass http errors to R

# Output:
out <- httr::content(r, "parsed", "application/json")[[1]]
message(out$stdout, paste0("score: ", round(out$score*100), "%")) # get back score + messages from codeOcean

# upload for final submission:
if(final)
{
message("For final submission, go to the openHPI task, open CodeOcean from there, click 'SCORE', then 'SUBMIT'.",
        "\nThis is currently not available from within R, sorry about the inconvenience.")
# exercise description potentially with openHPI URL:
desc <- paste0(dir, "/Exercise.txt")
if(file.exists(desc)) desc <- readLines(desc, warn=FALSE) else desc <- ""
url <- grep("\\[\\](.*)", desc, value=TRUE)
if(nchar(url)>0)
  {
	url <- gsub("[](", "", url, fixed=TRUE)
	url <- gsub(")$", "", url)
	message("At least I'm opening the task: ", url)
	browseURL(url)
	}
} # end final
# Output:
return(invisible(out))
}
