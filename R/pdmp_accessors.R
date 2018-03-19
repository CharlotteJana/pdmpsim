#======== todo =================================================================
### simecol slot "inputs" importieren?
### auch ... einfügen wie in simecol accessors (siehe ?parms)
### in simecol wurden nicht die generics, sondern die methoden exportiert (exportMethods in Namespace).
# wenn ich das mache, kommt es zu Fehlermeldungen bei R CMD check, ich muss die generics exportieren.
# Man muss anscheinend beides exportieren. Dann wird die Hilfeseite ewig lang!

#======== Description ===========

#' Accessor functions for Class pdmpModel
#'
#' Get or set the slot values for an object of class \code{\link{pdmpModel}}. 
#' In most cases, setting a new value also affects the slot \code{out} 
#' (where the simulation result can be stored). This applies to slots 
#' \code{times}, \code{parms}, \code{init}, \code{dynfunc}, \code{ratefunc},
#' \code{jumpfunc} and \code{initfunc}, which set out to NULL.
#' Setting a new value for \code{descr} will not affect the slot \code{out}.
#'
#' @param obj an object of class \code{\link{pdmpModel}}
#' @param value the value that shall be set
#'
#' @importMethodsFrom simecol "parms" "parms<-" "times" "times<-" 
#' @importMethodsFrom simecol "init" "init<-" "out" "out<-" "solver" "solver<-"
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
