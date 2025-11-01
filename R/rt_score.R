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
if(Sys.getenv("CODEOCEAN")=="true") return("Not running rt_score in browser-CodeOcean.")
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

# check if Rstudio is running (not the case in VScode or terminal)
is_rs <- try(rstudioapi::isAvailable(), silent=TRUE)
if(inherits(is_rs,"try-error")) is_rs <- FALSE
# Warn if files are changed but not saved:
if(is_rs) rt_check_for_unsaved_files(dir, warnonly=TRUE)

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
# rt_gives to suppress info: Initiating curl with CURL_SSL_BACKEND: openssl
r <- rt_gives_echo(try(httr::POST(url=co_url, body=body, config=httr::content_type("application/json")), silent=TRUE))$value
# catch some errors:
erm <- ifelse(inherits(r, "try-error"), r, httr::http_condition(r, "error")$message)
if(grepl("fatal SSL/TLS alert", erm))
  erm <- paste0(erm,"\nSebastian says: openssl must support TLS 1.3.
-------------
Berry says: run the following two lines in R:
  if(!requireNamespace('usethis', quietly=TRUE)) install.packages('usethis')
  usethis::edit_r_environ()
Then add the following, save the file and restart R (CTRL + SHIFT + F10, CMD + SHIFT + 0):
  CURL_SSL_BACKEND=openssl
-------------
Further info / alternative approaches:
  https://stackoverflow.com/a/64373553/1587132
  https://stackoverflow.com/a/71736921
  update git for windows for the newest openssl, set TLS 1.3 (link below) and restart PC.
  https://answers.microsoft.com/en-us/windows/forum/all/how-to-enable-tls-13-in-windows-10/f9ab4993-4758-4de3-a7f9-54a47b61cc77
")
if(grepl("Timeout was reached", erm) || grepl("Forbidden", erm)) # forbidden reported in the forum
	if(de)
	warning("Bist du \u00fcber ein VPN online? Versuche es nochmal ohne Proxy. Alternativ hilft vielleicht Folgendes:\n",
	'httr::set_config(httr::use_proxy(url="your.proxy.ip", port="port", username="user",password="pw"))', call.=FALSE)
  else
	warning("You might be connected through a VPN. Try again without a proxy. Alternatively, the following might help:\n",
	'httr::set_config(httr::use_proxy(url="your.proxy.ip", port="port", username="user",password="pw"))', call.=FALSE)
# stop for failures:
if(inherits(r, "try-error")) return(warning("httr::POST failed with: ", erm))

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
# Python output:
if(grepl("test01", out$stderr)){
mout <- paste(unlist(out$error_messages), collapse="\n")
# browser()
mout <- paste0(mout, "\nPassed tests: ", out$passed, " out of ", out$count,", Score: ", round(out$score*100), "%")
if(out$status=="timeout")            mout <- paste0(mout, "\nTesting your code took too long (",round(out$container_execution_time,1)," sec).")
if(out$status=="container_depleted") mout <- paste0(mout, "\nCodeOcean has an issue (container_depleted), please report to Berry.")
if(out$status=="out_of_memory")      mout <- paste0(mout, "\nYour code uses too much memory.")
mout <- paste0("---------------\nSCORING RESULTS\n---------------\n", mout)
message(mout) # print messages + score from codeOcean
return(invisible(if(fullout) r else out))
} # End Python
# R output (rest untill end of function):
mout <- out$stdout # message output
mout <- sub("Rscript.*tests.R\n", "", mout)
mout <- gsub("AssertionError: ", "-", mout, fixed=TRUE)
mout <- gsub("\r\n", "\n", mout) # without this, a trailing \r remains from the original \r\n
mout <- sub("\n$", "", mout)     # since my R 4.2.0 / httr 1.4.2 setup (Apr 2022)
mout <- gsub("\n\\s*\n\\s*", "\n", mout) # see note at end of rt_test_exercise
if(de) mout <- sub("(\\d{1,}) examples, (\\d{1,}) passed","\\1 Aufgaben, \\2 gel\u00F6st", mout)
mout <- paste0(mout, ", Score: ", round(out$score*100), "%")
if(out$status=="timeout")
  mout <- paste0(if(de) "Das Testen deines Codes hat zulange gedauert (" else
  "Testing your code took too long (", round(out$container_execution_time,1), " sec)", mout)
if(out$status=="container_depleted")
  mout <- paste0(mout, "\nCodeOcean has an issue (container_depleted), please report to Berry.")
if(out$status=="failed")
	{
	out2 <- out
	out2$stdout <- NULL
	out2$error_messages <- NULL
	print(str(out2))
	mout <- paste0(if(de) "Beim Testen deines Codes trat ein Problem auf:\n" else "A problem occured while testing:\n",
	  out$stderr, if(de) "Bitte informiere Berry.\nFEHLER" else "Please report this to Berry.\nERROR", mout)
  }
if(out$status=="out_of_memory")
	{                              # happens e.g. with table(airquality)
	mout <- paste0(if(de) "Dein Code verbraucht zuviel Arbeitsspeicher.\n" else "Your code uses too much memory.\n",
								 out$status, ", ", out$stderr, mout)
  }
if(trimws(out$stderr)!="")
	{
	mout <- paste0(if(de)"Beim Testen deines Codes trat eine Warnung / Ausgabe auf:\n" else "A warning / output occured while testing:\n",
		out$stderr, mout)
  }
message(mout) # print messages + score from codeOcean
return(invisible(if(fullout) r else out))
}
