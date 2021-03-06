% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdmpBorder_accessors.R
\docType{methods}
\name{pdmpBorder-accessors}
\alias{pdmpBorder-accessors}
\alias{borderfunc}
\alias{borderfunc<-}
\alias{borroot}
\alias{terroot}
\alias{borderfunc,pdmpBorder-method}
\alias{terroot,pdmpBorder-method}
\alias{borroot,pdmpBorder-method}
\alias{borderfunc<-,pdmpBorder-method}
\alias{borroot<-,pdmpBorder-method}
\alias{terroot<-,pdmpBorder-method}
\title{Accessor functions for Class pdmpBorder}
\usage{
borderfunc(obj)

borderfunc(obj) <- value

borroot(obj)

terroot(obj)

\S4method{borderfunc}{pdmpBorder}(obj)

\S4method{terroot}{pdmpBorder}(obj)

\S4method{borroot}{pdmpBorder}(obj)

\S4method{borderfunc}{pdmpBorder}(obj) <- value

\S4method{borroot}{pdmpBorder}(obj) <- value

\S4method{terroot}{pdmpBorder}(obj) <- value
}
\arguments{
\item{obj}{an object of class \code{\link{pdmpBorder}}}

\item{value}{the value that shall be set}
}
\description{
Get or set the slot values for an object of class \code{\link{pdmpBorder}}. 
In most cases, setting a new value also affects the slot \code{out} 
(where the simulation result can be stored). This applies to slots 
\code{borderfunc}, \code{borroot}, \code{terroot}, which set out to 
NULL. For the rest of the slots see \code{\link{pdmp-accessors}}.
}
\details{
It is also possible to modify the slots directly,
e.g. the parameters of a model \code{pdmp} with \code{pdmp@parms}, 
but this is normally not recommended as there is no guarantee that this 
will work in a compatible way in future versions.
}
