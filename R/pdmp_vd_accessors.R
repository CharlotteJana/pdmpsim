#======== todo =================================================================
#t2 simecol slot "inputs" importieren?
#t2 auch ... einfügen wie in simecol accessors (siehe ?parms)
#t3 Die Hilfe ist sehr lang, aber wenn ich weniger dokumentiere, kommen Fehler
#t3 wenn discStates geändert, verify aufrufen
#t1 discStates sollte out löschen, oder nicht?
#t3 warum muss ich eine generic für parms<- schreiben?

#======== Description ===========

#' Accessor functions for Class pdmp_vd_Model
#'
#' Get or set the slot values for an object of class \code{\link{pdmp_vd_Model}}. 
#' In most cases, setting a new value also affects the slot \code{out} 
#' (where the simulation result can be stored). This applies to slots 
#' \code{times}, \code{parms}, \code{init}, \code{dynfunc}, \code{ratefunc},
#' \code{jumpfunc} and \code{initfunc}, which set out to NULL.
#' Setting a new value for \code{descr} or \code{discStates} will not affect 
#' the slot \code{out}.
#' 
#' It is also possible to modify the slots directly,
#' e.g. the parameters of a model \code{pdmp} with \code{pdmp@parms}, 
#' but this is normally not recommended as there is no guarantee that this 
#' will work in a compatible way in future versions.
#'
#' @param obj an object of class \code{\link{pdmp_vd_Model}}
#' @param value the value that shall be set
#'
#' @importMethodsFrom simecol "times" "times<-" "solver" "solver<-"
#' @importMethodsFrom simecol "init" "init<-" "out" "out<-" "parms"
#' @importMethodsFrom simecol "main" "main<-"  "equations" "equations<-" 
#' @importMethodsFrom simecol "observer" "observer<-" "initfunc" "initfunc<-"
#' @include pdmp_vd_class.R
#' @name pdmp_vd-accessors
#' @seealso Class definition \code{\link{pdmp_vd_Model}}
NULL

#========= setGenerics ===========

#' @rdname pdmp_vd-accessors
#' @docType methods
#' @export
setGeneric("summaryfunc", function(obj) standardGeneric("summaryfunc"))

#' @rdname pdmp_vd-accessors
#' @export
setGeneric("summaryfunc<-", function(obj,value) standardGeneric("summaryfunc<-"))


#======= Getters ==========

#' @rdname pdmp_vd-accessors

#' @export
setMethod("summaryfunc", "pdmp_vd_Model", function(obj) obj@summaryfunc)


#======= Setters that don't change slot out =======

#' @rdname pdmp_vd-accessors
#' @export
setMethod("summaryfunc<-", "pdmp_vd_Model", function(obj, value) {
  obj@summaryfunc <- value
  out(obj) <- NULL
  invisible(obj)
})

