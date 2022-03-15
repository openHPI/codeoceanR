#' @title upload exercise to codeOcean
#' @description upload complete exercise, message CO Score results
#' @return [httr::content()] output of response to http request, invisibly.
#'         Or [httr::POST()] response, if `fullout=TRUE`.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2020
#' @seealso [rt_local_score] for teachers, [exercise example](https://github.com/openHPI/codeoceanR/tree/main/inst/extdata) on github
#' @keywords test
#' @importFrom httr POST content_type content status_code http_condition
#' @importFrom rjson toJSON
#' @importFrom utils browseURL str
#' @importFrom berryFunctions checkFile normalizePathCP
#' @export
#'
#' @param dir Path to (processed) exercise folder.
#'            Must contain ".co" and all the "script_n.R" files
#'            referenced there, with the changes by the student, saved.
#'            DEFAULT: "."
#' @param submit Submit grade to openHPI? Only to be set when called from [rt_submit()]!.
#' @param fullout Return full [httr::POST()] output, instead of
#'            [httr::content]`("parsed", "application/json")[[1]]`?
#'            Mainly for development debugging. DEFAULT: FALSE
#'
rt_score <- function(dir=".", submit=FALSE, fullout=FALSE)
{
# Avoid recursive posting, so we can have rt_score() in the exercise script:
if(!interactive()) return(NULL)
de <- rt_default_language=="de"

# Check directory and file
dir <- berryFunctions::normalizePathCP(dir)
berryFunctions::checkFile(dir)
cofile <- paste0(dir, "/.co")
# run rt_local_score in exercise development folder with *tests.R:
if(!file.exists(cofile) && length(dir(dir, pattern=".*tests\\.R"))>0 )
	{
	return(rt_local_score())
  }

# Warn if files are changed but not saved:
rt_check_for_unsaved_files(dir, warnonly=TRUE)

# get CO token + url + file IDs/names:
co <- rt_read_cofile(cofile)
co_token <- co$token
co_url   <- co$url
co_files <- co$files
if(submit) co_url <- sub("evaluate", "submit", co_url)
co_files$name <-  paste0(dir, "/", co_files$name)
berryFunctions::checkFile(co_files$name)

# Get all file contents:
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
r <- try(httr::POST(url=co_url, body=body, config=httr::content_type("application/json")), silent=TRUE)
if(inherits(r, "try-error"))
erm <- r else
erm <- httr::http_condition(r, "error")$message

if(grepl("Timeout was reached", erm)) # default timeout after 10 secs
	if(de)
	warning("Bist du \u00fcber ein VPN online? Versuche es nochmal ohne Proxy. Alternativ hilft vielleicht Folgendes:\n",
	'httr::set_config(httr::use_proxy(url="your.proxy.ip", port="port", username="user",password="pw"))', call.=FALSE)
  else
	warning("You might be connected through a VPN. Try again without a proxy. Alternatively, the following might help:\n",
	'httr::set_config(httr::use_proxy(url="your.proxy.ip", port="port", username="user",password="pw"))', call.=FALSE)
if(inherits(r, "try-error")) return(warning("httr::POST failed with: ", r))

if(httr::status_code(r) >= 300)
  {
	w <- httr::http_condition(r, "warning")$message
	w <- paste0(w, "\ncontent message: ", httr::content(r)$message)
	if(httr::status_code(r) == 503)
    if(de)
      w <- paste0(w, "\nVersuche, eine andere Aufgabe zu scoren, dann diese nochmal.")
    else
      w <- paste0(w, "\nTry scoring a different exercise, then score this one again.")
	warning(w, call.=FALSE)
  return(invisible(r))
  }
if(submit) return(r)

# Output:
out <- httr::content(r, "parsed", "application/json")[[1]]
mout <- out$stdout # message output
mout <- sub("Rscript.*tests.R\n", "", mout)
mout <- gsub("AssertionError: ", "- ", mout, fixed=TRUE)
mout <- gsub("\n$", "", mout)
if(de) mout <- sub("(\\d{1,}) examples, (\\d{1,}) passed","\\1 Aufgaben, \\2 gel\u00F6st", mout)
mout <- paste0(mout, ", Score: ", round(out$score*100), "%")
if(out$status=="timeout")
  mout <- paste0(if(de) "Das Testen deines Codes hat zulange gedauert (" else
  "Testing your code took too long (", round(out$container_execution_time,1), " sec)", mout)
if(out$status=="failed")
	{
	out2 <- out
	out2$stdout <- NULL
	out2$error_messages <- NULL
	print(str(out2))
	mout <- paste0(if(de) "Beim Testen deines Codes trat ein Problem auf:\n" else "A problem occured while testing:\n",
	  out$stderr, if(de) "Bitte informiere Berry.\nFEHLER" else "Please report this to Berry.\nERROR", mout)
  }
if(trimws(out$stderr)!="")
	{
	mout <- paste0(if(de)"Beim Testen deines Codes trat eine Warnung auf:\n" else "A warning occured while testing:\n",
		out$stderr, if(de)"Bitte informiere Berry.\n" else "Please report this to Berry.\n", mout)
  }
message(mout) # print messages + score from codeOcean
return(invisible(if(fullout) r else out))
}
