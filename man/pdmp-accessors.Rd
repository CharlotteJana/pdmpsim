% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdmp_accessors.R, R/pdmpBorder_accessors.R
\docType{methods}
\name{pdmp-accessors}
\alias{pdmp-accessors}
\alias{out}
\alias{out<-}
\alias{dynfunc}
\alias{dynfunc<-}
\alias{ratefunc}
\alias{ratefunc<-}
\alias{jumpfunc}
\alias{jumpfunc<-}
\alias{descr}
\alias{descr<-}
\alias{discStates}
\alias{discStates<-}
\alias{parms}
\alias{parms<-}
\alias{init}
\alias{init<-}
\alias{solver}
\alias{solver<-}
\alias{times}
\alias{times<-}
\alias{initfunc}
\alias{initfunc<-}
\alias{out,pdmpModel-method}
\alias{dynfunc,pdmpModel-method}
\alias{ratefunc,pdmpModel-method}
\alias{jumpfunc,pdmpModel-method}
\alias{descr,pdmpModel-method}
\alias{discStates,pdmpModel-method}
\alias{parms,pdmpModel-method}
\alias{init,pdmpModel-method}
\alias{solver,pdmpModel-method}
\alias{times,pdmpModel-method}
\alias{initfunc,pdmpModel-method}
\alias{out<-,pdmpModel-method}
\alias{dynfunc<-,pdmpModel-method}
\alias{ratefunc<-,pdmpModel-method}
\alias{jumpfunc<-,pdmpModel-method}
\alias{discStates<-,pdmpModel-method}
\alias{parms<-,pdmpModel-method}
\alias{init<-,pdmpModel-method}
\alias{solver<-,pdmpModel-method}
\alias{times<-,pdmpModel-method}
\alias{initfunc<-,pdmpModel-method}
\alias{descr<-,pdmpModel-method}
\alias{borroot<-}
\alias{terroot<-}
\title{Accessor functions for Class pdmpModel}
\usage{
out(obj, value)

out(obj) <- value

dynfunc(obj)

dynfunc(obj) <- value

ratefunc(obj)

ratefunc(obj) <- value

jumpfunc(obj)

jumpfunc(obj) <- value

descr(obj)

descr(obj) <- value

discStates(obj)

discStates(obj) <- value

parms(obj, value)

parms(obj) <- value

init(obj, value)

init(obj) <- value

solver(obj, value)

solver(obj) <- value

times(obj, value)

times(obj) <- value

initfunc(obj, value)

initfunc(obj) <- value

\S4method{out}{pdmpModel}(obj)

\S4method{dynfunc}{pdmpModel}(obj)

\S4method{ratefunc}{pdmpModel}(obj)

\S4method{jumpfunc}{pdmpModel}(obj)

\S4method{descr}{pdmpModel}(obj)

\S4method{discStates}{pdmpModel}(obj)

\S4method{parms}{pdmpModel}(obj)

\S4method{init}{pdmpModel}(obj)

\S4method{solver}{pdmpModel}(obj)

\S4method{times}{pdmpModel}(obj)

\S4method{initfunc}{pdmpModel}(obj)

\S4method{out}{pdmpModel}(obj) <- value

\S4method{dynfunc}{pdmpModel}(obj) <- value

\S4method{ratefunc}{pdmpModel}(obj) <- value

\S4method{jumpfunc}{pdmpModel}(obj) <- value

\S4method{discStates}{pdmpModel}(obj) <- value

\S4method{parms}{pdmpModel}(obj) <- value

\S4method{init}{pdmpModel}(obj) <- value

\S4method{solver}{pdmpModel}(obj) <- value

\S4method{times}{pdmpModel}(obj) <- value

\S4method{initfunc}{pdmpModel}(obj) <- value

\S4method{descr}{pdmpModel}(obj) <- value

borroot(obj) <- value

terroot(obj) <- value
}
\arguments{
\item{obj}{an object of class \code{\link{pdmpModel}}}

\item{value}{the value that shall be set}
}
\description{
Get or set the slot values for an object of class \code{\link{pdmpModel}}. 
In most cases, setting a new value also affects the slot \code{out} 
(where the simulation result can be stored). This applies to slots 
\code{times}, \code{parms}, \code{init}, \code{dynfunc}, \code{ratefunc},
\code{jumpfunc}, \code{discStates} and \code{initfunc}, which set out to 
NULL. Setting a new value for \code{descr} will not affect 
the slot \code{out}.
}
\details{
It is also possible to modify the slots directly,
e.g. the parameters of a model \code{pdmp} with \code{pdmp@parms}, 
but this is normally not recommended as there is no guarantee that this 
will work in a compatible way in future versions.
}
\seealso{
Class definition \code{\link{pdmpModel}}
}
