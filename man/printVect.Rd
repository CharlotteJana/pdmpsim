% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdmp_methods.R
\name{printVect}
\alias{printVect}
\title{printVect}
\usage{
printVect(v, collapse = ", ", sep = " = ")
}
\arguments{
\item{v}{a named numeric vector}

\item{collapse}{an optional character string to separate the results.  Not
    \code{\link{NA_character_}}.}

\item{sep}{a character string to separate the names and terms. 
Not \code{\link{NA_character_}}.}
}
\value{
a string containing the names and rounded values of the vector.
}
\description{
Convert a named numeric vector to a string. All numbers are rounded up to 2 
significant digits. This is a helpfunction for method \code{\link{format}}.
}
\examples{
pdmpsim:::printVect(c("foo" = 1/3, "bar" = sqrt(2)))
pdmpsim:::printVect(c("a" = 1, "b" = 2, "c" = 3), sep = "~", collapse = NULL)
}
\keyword{internal}
