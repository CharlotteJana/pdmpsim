#======== todo =================================================================
#t2 simecol slot "inputs" importieren?
#t2 auch ... einfügen wie in simecol accessors (siehe ?parms)
#t3 Die Hilfe ist sehr lang, aber wenn ich weniger dokumentiere, kommen Fehler
#t3 wenn discStates geändert, verify aufrufen
#t3 warum muss ich eine generic für parms<- schreiben?

#======== Description ===========

#' Accessor functions for Class pdmpModel
#'
#' Get or set the slot values for an object of class \code{\link{pdmpModel}}. 
#' In most cases, setting a new value also affects the slot \code{out} 
#' (where the simulation result can be stored). This applies to slots 
#' \code{times}, \code{parms}, \code{init}, \code{dynfunc}, \code{ratefunc},
#' \code{jumpfunc}, \code{discStates} and \code{initfunc}, which set out to 
#' NULL. Setting a new value for \code{descr} will not affect 
#' the slot \code{out}.
#' 
#' It is also possible to modify the slots directly,
#' e.g. the parameters of a model \code{pdmp} with \code{pdmp@parms}, 
#' but this is normally not recommended as there is no guarantee that this 
#' will work in a compatible way in future versions.
#'
#' @param obj an object of class \code{\link{pdmpModel}}
#' @param value the value that shall be set
#'
#' @importMethodsFrom simecol "times" "times<-" "solver" "solver<-"
#' @importMethodsFrom simecol "init" "init<-" "out" "out<-" "parms"
#' @importMethodsFrom simecol "main" "main<-"  "equations" "equations<-" 
#' @importMethodsFrom simecol "observer" "observer<-" "initfunc" "initfunc<-"
#' @include pdmp_class.R
#' @name pdmp-accessors
#' @seealso Class definition \code{\link{pdmpModel}}
NULL

#========= setGenerics ===========

#' @rdname pdmp-accessors
#' @export
setGeneric("dynfunc", function(obj) standardGeneric("dynfunc"))
#' @rdname pdmp-accessors
#' @export
setGeneric("dynfunc<-", function(obj, value) standardGeneric("dynfunc<-"))
#' @rdname pdmp-accessors
#' @export
setGeneric("ratefunc", function(obj) standardGeneric("ratefunc"))
#' @rdname pdmp-accessors
#' @export
setGeneric("ratefunc<-", function(obj, value) standardGeneric("ratefunc<-"))
#' @rdname pdmp-accessors
#' @export
setGeneric("jumpfunc", function(obj) standardGeneric("jumpfunc"))
#' @rdname pdmp-accessors
#' @export
setGeneric("jumpfunc<-", function(obj, value) standardGeneric("jumpfunc<-"))
#' @rdname pdmp-accessors
#' @export
setGeneric("descr", function(obj) standardGeneric("descr"))
#' @rdname pdmp-accessors
#' @export
setGeneric("descr<-", function(obj, value) standardGeneric("descr<-"))
#' @rdname pdmp-accessors
#' @export
setGeneric("discStates", function(obj) standardGeneric("discStates"))
#' @rdname pdmp-accessors
#' @export
setGeneric("discStates<-", function(obj, value) standardGeneric("discStates<-"))
#' @rdname pdmp-accessors
#' @export
setGeneric("parms<-", function(obj, value) standardGeneric("parms<-"))

#======= Getters ==========

#' @rdname pdmp-accessors
#' @export
setMethod("dynfunc", "pdmpModel", function(obj) obj@dynfunc)
#' @rdname pdmp-accessors
#' @export
setMethod("ratefunc", "pdmpModel", function(obj) obj@ratefunc)
#' @rdname pdmp-accessors
#' @export
setMethod("jumpfunc", "pdmpModel", function(obj) obj@jumpfunc)
#' @rdname pdmp-accessors
#' @export
setMethod("descr", "pdmpModel", function(obj) obj@descr)
#' @rdname pdmp-accessors
#' @export
setMethod("discStates", "pdmpModel", function(obj) obj@discStates)

#======= Setters that set obj@out <- NULL =========

#' @rdname pdmp-accessors
#' @export
setMethod("dynfunc<-", "pdmpModel", function(obj, value) {
  obj@dynfunc <- value
  out(obj) <- NULL
  invisible(obj)
})
#' @rdname pdmp-accessors
#' @export
setMethod("ratefunc<-", "pdmpModel", function(obj, value) {
  obj@ratefunc <- value
  out(obj) <- NULL
  invisible(obj)
})
#' @rdname pdmp-accessors
#' @export
setMethod("jumpfunc<-", "pdmpModel", function(obj, value) {
  obj@jumpfunc <- value
  out(obj) <- NULL
  invisible(obj)
})
#' @rdname pdmp-accessors
#' @export
setMethod("parms<-", "pdmpModel", function(obj, value) {
  obj@parms <- value
  out(obj) <- NULL
  invisible(obj)
})
#' @rdname pdmp-accessors
#' @export
setMethod("discStates<-", "pdmpModel", function(obj, value) {
  obj@discStates <- value
  out(obj) <- NULL
  invisible(obj)
})

#======= Setters that don't change slot out =======

#' @rdname pdmp-accessors
#' @export
setMethod("descr<-", "pdmpModel", function(obj, value) {
  obj@descr <- value
  invisible(obj)
})
#' @rdname pdmp-accessors
#' @export
setMethod("main<-", "pdmpModel", function(obj, value) {
  warning("Slot 'main' is a slot inherited by 'simObj' of class 'simecol' 
  and cannot be changed. Please modify slot 'dynfunc' instead'.")
  invisible(obj)
})
#' @rdname pdmp-accessors
#' @export
setMethod("equations<-", "pdmpModel", function(obj, value) {
  warning("Slot 'equations' is a slot inherited by 'simObj' of class 'simecol' 
  and cannot be changed.")
  invisible(obj)
})
#' @rdname pdmp-accessors
#' @export
setMethod("observer<-", "pdmpModel", function(obj, value) {
  warning("Slot 'observer' is a slot inherited by 'simObj' of class 'simecol' 
  and cannot be changed.")
  invisible(obj)
})
