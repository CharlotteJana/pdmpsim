 #======== Description ===========

#' Accessor functions for Class pdmpBorder
 #'
 #' Get or set the slot values for an object of class \code{\link{pdmpBorder}}. 
 #' In most cases, setting a new value also affects the slot \code{out} 
 #' (where the simulation result can be stored). This applies to slots 
 #' \code{borderfunc}, \code{borroot}, \code{terroot}, which set out to 
 #' NULL. For the rest of the slots see \code{\link{pdmp-accessors}}.
 #' 
 #' It is also possible to modify the slots directly,
 #' e.g. the parameters of a model \code{pdmp} with \code{pdmp@parms}, 
 #' but this is normally not recommended as there is no guarantee that this 
 #' will work in a compatible way in future versions.
#'
#' @param obj an object of class \code{\link{pdmpBorder}}
#' @param value the value that shall be set
#' @include pdmpBorder_class.R pdmpBorder_methods.R
#' @name pdmpBorder-accessors
#' @importFrom simecol "out<-"
NULL

#========= setGenerics ===========

#' @rdname pdmpBorder-accessors
setGeneric("borderfunc", function(obj) standardGeneric("borderfunc"))
#' @rdname pdmpBorder-accessors
#' @export
setGeneric("borderfunc<-", function(obj, value) standardGeneric("borderfunc<-"))
#' @rdname pdmpBorder-accessors
#' @export
setGeneric("borroot", function(obj) standardGeneric("borroot"))
#' @rdname pdmp-accessors
#' @export
setGeneric("borroot<-", function(obj, value) standardGeneric("borroot<-"))
#' @rdname pdmpBorder-accessors
#' @export
setGeneric("terroot", function(obj) standardGeneric("terroot"))
#' @rdname pdmp-accessors
#' @export
setGeneric("terroot<-", function(obj, value) standardGeneric("terroot<-"))
#' @rdname pdmpBorder-accessors
#' @export

#======= Getters ==========

#' @rdname pdmpBorder-accessors
#' @export
setMethod("borderfunc", "pdmpBorder", function(obj) obj@borderfunc)
#' @rdname pdmpBorder-accessors
#' @export
setMethod("terroot", "pdmpBorder", function(obj) obj@terroot)
#' @rdname pdmpBorder-accessors
#' @export
setMethod("borroot", "pdmpBorder", function(obj) obj@borroot)

#======= Setters that set obj@out <- NULL =========

#' @rdname pdmpBorder-accessors
#' @export
setMethod("borderfunc<-", "pdmpBorder", function(obj, value) {
  obj@borderfunc <- value
  out(obj) <- NULL
  invisible(obj)
})
#' @rdname pdmpBorder-accessors
#' @export
setMethod("borroot<-", "pdmpBorder", function(obj, value) {
  obj@borroot <- value
  out(obj) <- NULL
  invisible(obj)
})
#' @rdname pdmpBorder-accessors
#' @export
setMethod("terroot<-", "pdmpBorder", function(obj, value) {
  obj@terroot <- value
  out(obj) <- NULL
  invisible(obj)
})
