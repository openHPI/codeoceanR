% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rt_has_ncols.R
\name{rt_has_ncols}
\alias{rt_has_ncols}
\title{test object dimension}
\usage{
rt_has_ncols(df, len)
}
\arguments{
\item{df}{data.frame-like object to be tested. Just the name, not a character string.}

\item{len}{Value to which \link{ncol} is compared}
}
\value{
Logical: TRUE / FALSE depending on whether object has \code{len} columns.
}
\description{
Test ncol of object, calling \code{\link[=rt_warn]{rt_warn()}} with informative and helpful message if needed.
}
\seealso{
\link{rt_has_column}, \link{rt_has_nrows}, \link{rt_has_length}, \href{https://github.com/openHPI/codeoceanR/tree/main/inst/extdata}{exercise example} on github
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de}, Oct 2020
}
\keyword{error}
\keyword{test}
