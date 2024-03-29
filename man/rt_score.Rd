% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_score.R
\name{rt_score}
\alias{rt_score}
\title{upload exercise to codeOcean}
\usage{
rt_score(dir = ".", submit = FALSE, fullout = FALSE)
}
\arguments{
\item{dir}{Path to (processed) exercise folder.
Must contain ".co" and all the "script_n.R" files
referenced there, with the changes by the student, saved.
DEFAULT: "."}

\item{submit}{Submit grade to openHPI? Only to be set when called from \code{\link[=rt_submit]{rt_submit()}}!.}

\item{fullout}{Return full \code{\link[httr:POST]{httr::POST()}} output, instead of
\link[httr:content]{httr::content}\verb{("parsed", "application/json")[[1]]}?
Mainly for development debugging. DEFAULT: FALSE}
}
\value{
\code{\link[httr:content]{httr::content()}} output of response to http request, invisibly.
Or \code{\link[httr:POST]{httr::POST()}} response, if \code{fullout=TRUE}.
}
\description{
upload complete exercise, message CO Score results
}
\seealso{
\link{rt_local_score} for teachers, \href{https://github.com/openHPI/codeoceanR/tree/main/inst/extdata}{exercise example} on github
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de}, Oct 2020
}
\keyword{test}
