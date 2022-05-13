#' @title Submit score to CodeOcean and from there to openHPI
#' @description Submit score to CodeOcean and from there to openHPI
#' @return JSON array or hash. It can be analyzed e.g. with
#'         [httr::content]`(r, "parsed", "application/json")`
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2020
#' @seealso first run [rt_score()]
#' @export
#' @importFrom httr status_code http_condition stop_for_status content
#'
#' @param dir Path to (processed) exercise folder.
#'            Must contain ".co" and all the "script_n.R" files
#'            referenced there, with the changes by the student, saved.
#'            DEFAULT: "."
#' @param confirm Logical: ask for confirmation before submission? DEFAULT: TRUE
#'
rt_submit <- function(dir=".", confirm=TRUE)
{
# Avoid error in case students leave rt_submit() in the exercise script
# on the CO server (non-interactive mode), readline returns ""
if(Sys.getenv("CODEOCEAN")=="true")
	return("Not running 'rt_submit' in browser-CodeOcean.")

de <- rt_default_language=="de"
if(confirm) if(de)
{
rl <- readline("Dies ist meine \u00dcbertragung des Punktestandes an openHPI (j/n): ")
if(!tolower(substr(rl,1,1)) %in% c("y","j")) stop("Die Einreichung wurde abgebrochen.")
} else
{
rl <- readline("This is my grade submission to openHPI (y/n): ")
if(!tolower(substr(rl,1,1)) %in% c("y","j")) stop("Submission has been canceled.")
}

r <- rt_score(dir, submit=TRUE)
if(is.null(r)) stop("rt_score result is NULL, probably rt_local_score has been invoked.")

# according to Sebastian Serth, r is always JSON:
# - unlikely: array like for rt_score() if submission went wrong before running the tests
# - normally: hash with the keys 'message' + 'status' (HTTP-Statuscode) + 'score'

# Add info to http errors (if any):
# https://github.com/openHPI/codeocean/blob/master/app/controllers/remote_evaluation_controller.rb
# https://httpstatuses.com/

erm <- httr::http_condition(r, "error")$message
if(!httr::status_code(r) %in% c(202, 207))
	if(de)
	warning("Scheinbar gab es einen Fehler bei der Punkte\u00fcbertragung, Pardon!",
					"\nSende diese (vollst\u00E4ndige) Nachricht bitte an Berry.\n",
					toString(erm), call.=FALSE)
  else
  warning("Looks like something went wrong in the submission process. Sorry! ",
					"\nPlease send this (complete) message to Berry.\n",
					toString(erm), call.=FALSE)
httr::stop_for_status(r) # if any, pass http errors to R

# Message + score from codeOcean
out <- httr::content(r, "parsed", "application/json")
if(de)
message(out$message, "\nDie \u00fcbertragene Bewertung ist ", round(out$score,2), "%.")
else
message(out$message, "\nThe submitted score is ", round(out$score,2), "%.")

# Output
return(invisible(r))
}
