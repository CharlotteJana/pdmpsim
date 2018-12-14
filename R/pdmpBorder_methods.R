
#' @include pdmp_class.R pdmp_methods.R pdmpBorder_class.R pdmpBorder_accessors.R
NULL

#---------- print --------------

#' Methods for funktion print in package \pkg{pdmpsim}
#' 
#' @param x an object of class \code{pdmpModel} or one of its subclasses.
#' @param all speciefies whether all slots are printed. If FALSE (the default),
#' only the slots that characterize the model will be printed.
#' @param ... optional parameters passed to print.
#' @importFrom utils str
#' @export
setMethod(f = "print",
          signature = "pdmpBorder",
          definition = function(x, all = FALSE, ...){
            #  .local <- function (x, all = FALSE, ...)
            #  {
            if (all) {
              print.default(x, all = TRUE, ...)
            }
            else {
              cat("An S4-object of class", class(x)[1], "\n\n")
              slotnames <- slotNames(x)
              for (slotname in slotnames) {
                slotcontent <- slot(x, slotname)
                if (!is.null(slotcontent)) {
                  if (slotname == "main") next
                  if (slotname == "solver") next
                  cat("Slot ", dQuote(slotname), ":\n", sep = "")
                  if (slotname == "out") cat("  outputs exist ...\n")
                  else if (slotname == "parms")  utils::str(slotcontent)
                  else print(slotcontent, ...)
                  cat("\n")
                }
              }
              cat("Hint: use print(x, all=TRUE) to see all pdmpModel slots.\n")
            }
            #  }
            #  .local(x, ...)
          })

#---------------- plot -----------------

#' Plot a PDMP
#' 
#' This is method plots single simulations of
#' piecewise deterministic markov processes defined as
#' \code{\link{pdmpBorder}}. There are also other plot
#' methods available that use \pkg{ggplot2}. 
#'
#' @param x an object of class pdmpBorder with a simulation 
#' stored in slot \code{out}
#' @param y ignored
#' @param ... optional plotting parameters
#' @seealso \code{\link{plotSeeds}} for another plot function
#' to plot single simulations
#' @importFrom graphics title
#' @export
setMethod("plot", signature(x="pdmpBorder", y="missing"),
          function(x, y, ...) {
            if (is.null(x@out))
              stop("Please simulate the model before plotting", call. = FALSE)
            par(oma = c(0,0,2,0))
            do.call("plot", alist(x@out, ...))
            graphics::title(x@descr, line = -0.3, outer = TRUE)
          }
)

