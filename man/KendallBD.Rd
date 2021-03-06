% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{KendallBD}
\alias{KendallBD}
\title{Kendalls birth-and-death process}
\format{An object of class \code{\link{mjpModel}}.}
\usage{
KendallBD
}
\description{
This is a simple example for a markov jump process
defined as \code{\link{mjpModel}}. It is included to the package for 
demonstration purposes.
}
\section{Slots}{

\describe{
\item{\code{parms}}{birth and death rate.}

\item{\code{init}}{There is one integer  variable \code{N} with initial value 1.}

\item{\code{jumpfunc}}{There are two jumptypes. Births  jump from \code{N} to 
\code{N + 1}, deaths from \code{N} to \code{N - 1}.}

\item{\code{ratefunc}}{A vector of length two determining the probability of a jump 
being a birth or a death.}

\item{\code{times}}{The simulations will start at time \code{t = 0} and end at 
\code{t = 10} with step length 0.01.}
}}

\examples{
## the code used to generate this model:

KendallBD <- mjpModel(
    descr = "Kendall's birth-death process",
    init = c(N=10),
    times = c(from = 0, to = 10, by = 0.01),
    ratefunc = function(t, x, parms) parms[1:2]*x,
    jumpfunc = function(t, x, parms, jtype){
         x+switch(jtype,1,-1)
    },
    parms=c(b=1,d=1))

## load it and plot a simulation:
data("KendallBD")
plot(sim(KendallBD))
}
\seealso{
\code{\link{SIRstoch}} for a more sophisticated example of a 
\code{mjpModel} and \code{\link{mjpModel-class}}
for the formal description of the S4 class.
}
\keyword{datasets}
