#' @title upload exercise to codeOcean
#' @description upload complete exercise, message CO Score results
#' @return [httr::content()] output of response to http request, invisibly
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_local_score] for teachers, [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords test
#' @importFrom berryFunctions l2df
#' @importFrom httr POST content_type stop_for_status content
#' @export
#'
#' @param dir Path to (processed) exercise folder.
#'            Must contain ".co" and all the "script_n.R" files
#'            referenced there, with the changes by the student, saved.
#'            DEFAULT: "."
#'
rt_score <- function(dir=".")
{
# Check directory and file
dir <- berryFunctions::normalizePathCP(dir)
berryFunctions::checkFile(dir)
cofile <- paste0(dir, "/.co")
berryFunctions::checkFile(cofile)

# get CO token + url + file IDs
co <- readLines(cofile)

# Get all file contents:
cf <- co[-(1:2)]
if(length(cf)<1) stop("No filenames found to be submitted in file '.co'")
cf <- berryFunctions::l2df(strsplit(cf, "="))
colnames(cf) <- c("name", "id")
#
get_escaped_file_content <- function(fn)
  {
  d <- paste(readLines(fn), collapse="\n")
  d <- gsub("\\", "\\\\", d, fixed=TRUE)
  d <- gsub("\n", "\\n", d, fixed=TRUE)
  d <- gsub('\"', '\\"', d, fixed=TRUE)
  d <- gsub("'", "\\'", d, fixed=TRUE)
  d
  }
fileattr <- sapply(1:nrow(cf), function(i) paste0('{"file_id": ',cf$id[i],
							',"content": "',get_escaped_file_content(cf$name[i]),'"}'))
fileattr <- paste(fileattr[1], collapse=", ")

# put into http request body:
body <- paste0('{"remote_evaluation": {"validation_token": "',co[1],
							 '","files_attributes": [',fileattr,']}}')

# Submit to CodeOcean:
r <- httr::POST(url=co[2], body=body, httr::content_type("application/json"))
httr::stop_for_status(r) # if any, pass http errors to R

# Output:
out <- httr::content(r, "parsed", "application/json")[[1]]
message(out$stdout, paste0("score: ", round(out$score*100), "%")) # get back score + messages from codeOcean
return(invisible(out))
}
