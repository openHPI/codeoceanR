% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_submit.R
\name{rt_submit}
\alias{rt_submit}
\title{Submit score to CodeOcean and from there to openHPI}
\usage{
rt_submit(dir = ".", confirm = TRUE)
}
\arguments{
\item{dir}{Path to (processed) exercise folder.
Must contain ".co" and all the "script_n.R" files
referenced there, with the changes by the student, saved.
DEFAULT: "."}

\item{confirm}{Logical: ask for confirmation before submission? DEFAULT: TRUE}
}
\value{
JSON array or hash. It can be analyzed e.g. with
\link[httr:content]{httr::content}\verb{(r, "parsed", "application/json")}
}
\description{
Submit score to CodeOcean and from there to openHPI
}
\seealso{
first run \code{\link[=rt_score]{rt_score()}}
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de}, Nov 2020
}
