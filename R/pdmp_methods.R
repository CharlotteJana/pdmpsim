#======== todo =================================================================

#' @include pdmp_class.R
NULL

#---------- private methods -----------

#' printVect
#' 
#' Convert a named numeric vector to a string. All numbers are rounded up to 2 
#' significant digits. This is a helpfunction for method \code{\link{format}}.
#' 
#' @param v a named numeric vector
#' @param sep a character string to separate the names and terms. 
#' Not \code{\link{NA_character_}}.
#' @inheritParams base::paste 
#' @return a string containing the names and rounded values of the vector.
#' @examples
#' pdmpsim:::printVect(c("foo" = 1/3, "bar" = sqrt(2)))
#' pdmpsim:::printVect(c("a" = 1, "b" = 2, "c" = 3), sep = "~", collapse = NULL)
#' @keywords internal
printVect <- function(v, collapse = ", ", sep = " = "){
  if(length(v) == 0) return(NULL)
  if(is.numeric(v)) v <- signif(v, digits = 2)
  paste(vapply(seq_along(v), 
               function(i) paste(names(v)[i], v[[i]], sep = sep), 
               character(1)), 
        collapse = collapse)
}

#' Format a PDMP for pretty printing.
#'
#' Give important information about a \code{\link{pdmpModel}} in one character 
#' string. This method is used internally to generate default filenames or plot 
#' titles.
#' 
#' @param x an object of class \code{\link{pdmpModel}} or one of its subclasses.
#' @param slots a vector specifying the names of all slots that shall be pasted
#' to the string. ' The default is \code{c("parms", "init", "times")}. 
#' Supported slots are "descr", "parms", "init", "times" and "discStates".
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
#' data("toggleSwitch")
#' format(toggleSwitch, begin = "ToggleSwitch__", end = ".rda")
#' format(toggleSwitch, begin = paste0(descr(toggleSwitch), ": "), short=FALSE)
#' parms(toggleSwitch) <- list()
#' cat(format(toggleSwitch, short = FALSE, collapse = ".\n",
#'            slots = c("init", "times", "discStates"),
#'            begin = "A model without parameters:\n"))
#' 
#' @aliases format
#' @export
setMethod(f = "format", 
          signature = "pdmpModel", 
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
  if("discStates" %in% slots){
   statesName <- ifelse(short, "discStates_", "Discrete States: ")
   statesVals <- ""
   for(i in seq_along(discStates(x))){
     statesVals <- paste0(statesVals, 
                         names(discStates(x))[i],
                         ifelse(short, "", " \U220A "),
                         "{",
                         paste(discStates(x)[[i]], collapse = ","),
                         "}")
     if(i != length(discStates(x)) & !short)
       statesVals <- paste0(statesVals, ", ")
   }
   result <- paste0(result, paste0(statesName, statesVals, collapse))
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
#' @param x an object of class \code{pdmpModel} or one of its subclasses.
#' @param all speciefies whether all slots are printed. If FALSE (the default),
#' only the slots that characterize the model will be printed.
#' @param ... optional parameters passed to print.
#' @importFrom utils str
#' @export
setMethod(f = "print",
          signature = "pdmpModel",
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
#' \code{\link{pdmpModel}}. There are also other plot
#' methods available that use \pkg{ggplot2}. 
#'
#' @param x an object of class pdmpModel with a simulation 
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
setMethod("plot", signature(x="pdmpModel", y="missing"),
          function(x, y, ...) {
            if (is.null(x@out))
              stop("Please simulate the model before plotting", call. = FALSE)
            o.par <- par(oma = c(0,0,2,0))
            do.call("plot", alist(x@out, ...))
            graphics::title(x@descr, line = -0.3, outer = TRUE)
            par(o.par)
          }
)

