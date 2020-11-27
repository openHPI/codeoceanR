#' @title Submit score to CodeOcean and from there to openHPI
#' @description Submit score to CodeOcean and from there to openHPI
#' @return JSON array or hash. It can be analyzed e.g. with
#'         [httr::content]`(r, "parsed", "application/json")`
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2020
#' @seealso first run [rt_score()]
#' @export
#'
#' @param dir Path to (processed) exercise folder.
#'            Must contain ".co" and all the "script_n.R" files
#'            referenced there, with the changes by the student, saved.
#'            DEFAULT: "."
#'
rt_submit <- function(dir=".")
{
rl <- readline("This is irreversibly my final grade submission (y/n): ")
if(tolower(substr(rl,1,1)) != "y") stop("Submission has been canceled.")

r <- rt_score(dir, submit=TRUE)
# r is according to Sebastian Serth always JSON: either
# - array like for rt_score()    or
# - hash with the keys 'message' and 'status' (HTTP-Statuscode)

# Message
out <- httr::content(r, "parsed", "application/json")[[1]]
message(out) # print message from codeOcean

# Output
return(invisible(r))
}
