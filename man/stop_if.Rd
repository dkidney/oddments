% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{stop_if}
\alias{stop_if}
\title{Stop if condition is TRUE}
\usage{
stop_if(expr)
}
\arguments{
\item{expr}{an expression representing a condition to be tested - if it evaluates to
\code{TRUE} then an error will be thrown.}
}
\description{
This is intended to be the inverse of \link[base]{stopifnot}.
}
\examples{
\dontrun{

x = "abc"
stop_if(nchar(x) >= 4)
stop_if(nchar(x) <= 3)
}
}
