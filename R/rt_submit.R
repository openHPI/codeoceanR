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
#'
rt_submit <- function(dir=".")
{
rl <- readline("This is my final grade submission (y/n): ")
if(tolower(substr(rl,1,1)) != "y") stop("Submission has been canceled.")

r <- rt_score(dir, submit=TRUE)
if(is.null(r)) return(NULL) # For non-interactive session

# according to Sebastian Serth, r is always JSON:
# - unlikely: array like for rt_score() if submission went wrong before running the tests
# - normally: hash with the keys 'message' + 'status' (HTTP-Statuscode) + 'score'

# Add info to http errors (if any):
# https://github.com/openHPI/codeocean/blob/master/app/controllers/remote_evaluation_controller.rb

erm <- httr::http_condition(r, "error")$message
if(!httr::status_code(r) %in% c(202, 207))
	warning("Looks like something went wrong in rt_submit. Sorry! ",
					"\nPlease open the CO task from within openHPI, click score, then submit.",
					"\nThen send to following message to Berry, so it can be avoided next time.\n",
					toString(erm), call.=FALSE)
httr::stop_for_status(r) # if any, pass http errors to R

# Message + score from codeOcean
out <- httr::content(r, "parsed", "application/json")
message(out$message, "\nThe submitted score is ", round(out$score,2), "%.",
				"\nFeel free to continue the quiz including rt_score(), ",
				"but don't submit again as that will decrease your grade.")

# Output
return(invisible(r))
}
