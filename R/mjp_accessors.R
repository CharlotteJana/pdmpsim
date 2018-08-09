#======== todo =================================================================
#t1 


#======== Description ===========

#' Accessor functions for Class mjpModel
#'
#' Get or set the slot values for an object of class \code{\link{mjpModel}}. 
#' In most cases, setting a new value also affects the slot \code{out} 
#' (where the simulation result can be stored). This applies to slots 
#' \code{times}, \code{parms}, \code{init}, \code{ratefunc},
#' \code{jumpfunc} and \code{initfunc}, which set out to NULL.
#' Setting a new value for \code{descr} will not affect 
#' the slot \code{out}.
#' 
#' It is also possible to modify the slots directly,
#' e.g. the parameters of a model \code{mjp} with \code{mjp@parms}, 
#' but this is normally not recommended as there is no guarantee that this 
#' will work in a compatible way in future versions.
#'
#' @param obj an object of class \code{\link{mjpModel}}
#' @param value the value that shall be set
#'
#' @importMethodsFrom simecol "times" "times<-" "solver" "solver<-"
#' @importMethodsFrom simecol "init" "init<-" "out" "out<-" "parms"
#' @importMethodsFrom simecol "main" "main<-"  "equations" "equations<-" 
#' @importMethodsFrom simecol "observer" "observer<-" "initfunc" "initfunc<-"
#' @include mjp_class.R
#' @name mjp-accessors
#' @seealso Class definition \code{\link{mjpModel}}
NULL

#Setgeneric-already in pdmp

#======= Getters ==========

#' @rdname mjp-accessors
#' @export
setMethod("dynfunc", "mjpModel", function(obj) obj@dynfunc)
#' @rdname mjp-accessors
#' @export
setMethod("ratefunc", "mjpModel", function(obj) obj@ratefunc)
#' @rdname mjp-accessors
#' @export
setMethod("jumpfunc", "mjpModel", function(obj) obj@jumpfunc)
#' @rdname mjp-accessors
#' @export
setMethod("descr", "mjpModel", function(obj) obj@descr)


#======= Setters that set obj@out <- NULL =========

#' @rdname mjp-accessors
#' @export
setMethod("ratefunc<-", "mjpModel", function(obj, value) {
  obj@ratefunc <- value
  out(obj) <- NULL
  invisible(obj)
})
#' @rdname mjp-accessors
#' @export
setMethod("jumpfunc<-", "mjpModel", function(obj, value,jtype) {
  obj@jumpfunc <- value
  out(obj) <- NULL
  invisible(obj)
})
#' @rdname mjp-accessors
#' @export
setMethod("parms<-", "mjpModel", function(obj, value) {
  obj@parms <- value
  out(obj) <- NULL
  invisible(obj)
})

#======= Setters that don't change slot out =======

#' @rdname mjp-accessors
#' @export
setMethod("discStates<-", "mjpModel", function(obj, value) {
  obj@discStates <- value
  invisible(obj)
})
#' @rdname mjp-accessors
#' @export
setMethod("descr<-", "mjpModel", function(obj, value) {
  obj@descr <- value
  invisible(obj)
})
#' @rdname mjp-accessors
#' @export
setMethod("main<-", "mjpModel", function(obj, value) {
  warning("Slot 'main' is a slot inherited by 'simObj' of class 'simecol' 
  and cannot be changed. Please modify slot 'dynfunc' instead'.")
  invisible(obj)
})
#' @rdname mjp-accessors
#' @export
setMethod("equations<-", "mjpModel", function(obj, value) {
  warning("Slot 'equations' is a slot inherited by 'simObj' of class 'simecol' 
  and cannot be changed.")
  invisible(obj)
})
#' @rdname mjp-accessors
#' @export
setMethod("observer<-", "mjpModel", function(obj, value) {
  warning("Slot 'observer' is a slot inherited by 'simObj' of class 'simecol' 
  and cannot be changed.")
  invisible(obj)
})
