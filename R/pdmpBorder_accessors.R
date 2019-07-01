 #======== Description ===========

#' Accessor functions for Class pdmpBorder
#' 
#' erster satz...
#'
#' @param obj an object of class \code{\link{pdmpBorder}}
#' @param value the value that shall be set
#' @include pdmpBorder_class.R pdmpBorder_methods.R
#' @name pdmpBorder-accessors
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
  obj@dynfunc <- value
  out(obj) <- NULL
  invisible(obj)
})
#' @rdname pdmpBorder-accessors
#' @export
setMethod("borroot<-", "pdmpBorder", function(obj, value) {
  obj@ratefunc <- value
  out(obj) <- NULL
  invisible(obj)
})
#' @rdname pdmpBorder-accessors
#' @export
setMethod("terroot<-", "pdmpBorder", function(obj, value) {
  obj@jumpfunc <- value
  out(obj) <- NULL
  invisible(obj)
})
