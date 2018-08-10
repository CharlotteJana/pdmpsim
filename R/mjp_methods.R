#======== todo =================================================================
#t1 Plot methode wird nicht exportiert. Warum nicht???
#t2 examples anpassen
#' @include  pdmp_class.R pdmp_methods.R mjp_class.R mjp_accessors.R
NULL


#' Format a MJP for pretty printing.
#'
#' Give important information about a \code{\link{mjpModel}} in one character 
#' string. This method is used internally to generate default filenames or plot 
#' titles.
#' 
#' @param x an object of class \code{\link{mjpModel}} or one of its subclasses.
#' @param slots a vector specifying the names of all slots that shall be pasted
#' to the string. ' The default is \code{c("parms", "init", "times")}. 
#' Supported slots are "descr", "parms", "init",  and "times".
#' @param begin a character string that is pasted at the beginning.
#' @param end a character string that is pasted at the end.
#' @param short logical. If TRUE, a shorter version without space characters
#' will be returned (useful to generate filenames). Defaults to TRUE.
#' @param sep a character string to separate the names and values of slots that 
#' are vectors or lists (e. g. "times" or "parms"). It defaults to \code{"="} 
#' if \code{short} is \code{TRUE} and to \code{" = "} if \code{short} is 
#' \code{FALSE}.
#' @param collapse a character string to separate the slots.
#' It defaults to \code{"___"} if \code{short} is \code{TRUE} and to \code{". "} 
#' if \code{short} is \code{FALSE}.
#'
#' @examples
#' data("KendallBD")
#' format(KendallBD, begin = "ToggleSwitch__", end = ".rda")
#' format(KendallBD, begin = paste0(descr(toggleSwitch), ": "), short=FALSE)
#' parms(KendallBD) <- list()
#' cat(format(KendallBD, short = FALSE, collapse = ".\n",
#'            slots = c("init", "times","parms"),
#'            begin = "Kendalls birth-death-process:\n"))
#' 
#' @aliases format format,mjpModel-method
#' @export
setMethod(f = "format", 
          signature = "mjpModel", 
          definition = function(x, begin = NULL, end = NULL, short = TRUE, 
                                slots = c("parms", "init", "times"), 
                                sep, collapse){
  
  if(missing(collapse)) 
    collapse <- ifelse(short, "___", ". ")
  if(missing(sep))
    sep <- ifelse(short, "=", " = ")
  blank <- ifelse(short, "_", " ")
  vectCollapse <- ifelse(short, "_", ", ")
  
  result <- ifelse(is.null(begin), "", begin)
  
  if("descr" %in% slots){
    descrName <- ifelse(short, "", "Description: ")
    descrVals <- descr(x)
    result <- paste0(result, paste0(descrName, descrVals, collapse))
  }
  if("parms" %in% slots & length(parms(x)) != 0){
    parmsName <- ifelse(short, "Parms_", ifelse(length(parms(x)) == 1, 
                                                "Parameter: ", 
                                                "Parameters: "))
    parmsVals <- printVect(parms(x), sep = sep, collapse = vectCollapse)
    result <- ifelse(is.null(parmsVals), 
                     result, 
                     paste0(result, paste0(parmsName, parmsVals, collapse)))
  }
  if("init" %in% slots){
    initName <- ifelse(short, "Init_", ifelse(length(init(x)) == 1, 
                                              "Initial Value: ", 
                                              "Initial Values: "))
    initVals <- printVect(init(x), sep = sep, collapse = vectCollapse)
    result <- paste0(result, paste0(initName, initVals, collapse))
  }
  if("times" %in% slots){
    timeName <- ifelse(short, "Times_", "Times: ")
    if(short)
      timeVals <- paste0(times(x)["from"], blank, times(x)["to"], 
                         blank, times(x)["by"])
    else
      timeVals <- printVect(times(x), sep = sep, collapse = vectCollapse)
    result <- paste0(result, paste0(timeName, timeVals, collapse))
  }
  
  # remove 'collapse' at the end of the string:
  result <- substr(result, 1, nchar(result) - nchar(collapse))
  result <- paste0(result, end)
  return(result)
})

#---------- print --------------

#' Methods for funktion print in package \pkg{pdmpsim}
#' 
#' @param x an object of class \code{mjpModel} or one of its subclasses.
#' @param all speciefies whether all slots are printed. If FALSE (the default),
#' only the slots that characterize the model will be printed.
#' @param ... optional parameters passed to print.
#' @importFrom utils str
#' @export
setMethod(f = "print",
          signature = "mjpModel",
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
              cat("Hint: use print(x, all=TRUE) to see all mjpModel slots.\n")
              }
          #  }
          #  .local(x, ...)
})

#---------------- plot -----------------

#' Plot a PDMP
#' 
#' This is method plots single simulations of
#' markov jump processes defined as
#' \code{\link{mjpModel}}. There are also other plot
#' methods available that use \pkg{ggplot2}. 
#'
#' @param x an object of class mjpModel with a simulation 
#' stored in slot \code{out}
#' @param y ignored
#' @param ... optional plotting parameters
#' @examples 
#' data("toggleSwitch")
#' sim <- sim(toggleSwitch, seed = 1)
#' plot(sim, col = "red", lwd = 2)
#' 
#' # Alternative: plotSeeds
#' msim <- multSim(toggleSwitch, seeds = 1)
#' plot <- plotSeeds(msim)
#' plot + ggplot2::facet_grid(variable ~ seed)
#' @seealso \code{\link{plotSeeds}} for another plot function
#' to plot single simulations
#' @importFrom graphics title
#' @export
setMethod("plot", signature(x="mjpModel", y="missing"),
          function(x, y, ...) {
            if (is.null(x@out))
              stop("Please simulate the model before plotting", call. = FALSE)
            par(oma = c(0,0,2,0))
            do.call("plot", alist(x@out, ...))
            graphics::title(x@descr, line = -0.3, outer = TRUE)
          }
)

