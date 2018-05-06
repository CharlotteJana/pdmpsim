#======== todo =================================================================
#t2 NA vor den Plots ausführen
#t3 plot: subtitle: anzahl der Seeds mit Hinschreiben

#' @include pdmp_class.R pdmp_sim.R multSim.R multSimData_outputMethods.R
NULL

#### output methods #####

#' @importFrom utils str
#' @importFrom stats median
#' @export
print.multSim <- function(x, ...){
  cat("An S3-object of class", class(x)[1], "with elements \n\n")
  names <- names(x)
  for (name in names) {
    content <- x[name]
    if (!is.null(content)) {
      if (name == "seeds") {cat("$seeds\n"); utils::str(x$seeds)}
      if (name == "timeList") {
        times <- vapply(x$timeList, function(i) i[1:3], numeric(3))
        if(is.null(x$timeList)) cat("$timeList\nNULL\n")
        else{
          cat("$timeList\n\n time per simulation (median):\n")
          print(apply(times, 1, stats::median, na.rm = TRUE))
          cat("\n time altogether:\n")
          print(rowSums(times))
        }
      }
      if (name == "outputList"){
        bool <- vapply(x$outputList, function(i) is(i, "deSolve"), logical(1))
        h <- which(bool)
        r <- which(diff(h) != 1)
        if(all(bool)) cat("$outputList\n outputs exist for all seeds\n")
        else if(all(!bool)) cat("$outputList\n outputs don't exist\n")
        else{
          a <- x$seeds[h[sort(c(1, r, r + 1, length(h)))]]
          cat("$outputList\n outputs exist for seeds ", 
               paste(apply(matrix(a, nrow = 2), 2, paste, collapse = " - "), 
                     collapse = ", "), "\n")
        }
        #else if(table(bool)["TRUE"] > table(bool)["FALSE"])  
        #  cat("$outputs\n  outputs exist for all seeds except seed =", 
        #      x$seeds[!bool],"\n")
        #else 
        #  cat("$outputs\n  outputs exist for seed =", x$seeds[bool],"\n")
      }
      if (name == "model") 
        cat("$model\n", format(x$model, short = FALSE, 
            slots = c("descr", "parms", "init", "times"), collapse = "\n "))
      cat("\n")
    }
  }
}

#' @export
summary.multSim <- function(object, 
                            entries = 1:(min(5,length(object$seeds))), 
                            ...){
  for(i in entries){
    cat("\nseed =", object$seeds[i],":\n\n")
    print(object$timeList[[i]])
    cat("\n")
    print(summary(object$outputList[[i]]))
  }
  if(length(entries) != length(object$seeds)){
    cat("\nOnly", length(entries) ,"out of", length(object$seeds), 
        "simulations are shown.")
  }
}

#' @rdname hist
#' @export
hist.multSim <- function(x, t, main, ...){
  if(missing(main)) main <- x$model@descr
  data <- getMultSimData(x, times = t)
  hist(data, t = t, main = main, ...)
}

#' @rdname density
#' @export
density.multSim <- function(x, t, main, ...){
  if(missing(main)) main <- x$model@descr
  # timeText <- ifelse(length(t) == 1, 
  #                    paste("at time t =", t), 
  #                    "at different times")
  data <- getMultSimData(x, times = t)
  density(data, main = main, ...)
}

#' @param seeds vector with seed numbers to plot 
#' (only if x is a \code{\link{multSim}} Object)
#' @rdname plotSeeds
#' @importFrom ggplot2 ggtitle
#' @export
plotSeeds.multSim <- function(x, seeds, ...){
  if(missing(seeds)) seeds <- x$seeds
  data <- getMultSimData(x, seeds = seeds)
  plot <- plotSeeds(data, ...) + ggplot2::ggtitle(x$model@descr)
  return(plot)
}

#' @rdname plotTimes
#' @importFrom ggplot2 ggtitle
#' @export
plotTimes.multSim <- function(x, vars, times, threshold = NULL, 
                              plottype = "boxplot", ...){
  if(missing(vars)) 
    vars <- names(x$model@init)
  if(missing(times)){
    t <- fromtoby(x$model@times)
    times <- t[seq(1, length(t), length.out = 10)]
  }
  data <- getMultSimData(x, times = times)
  plot <- plotTimes(data, times = times, vars = vars, 
                    threshold = threshold, plottype = plottype, ...) + 
          ggplot2::ggtitle(x$model@descr)
  return(plot)
}

#' @rdname plotStats
#' @export
plotStats.multSim <- function(x, vars, funs, ...){
 if(missing(vars)) vars <- names(x$model@init) 
 data <- getMultSimData(x)
 plotStats(data, vars, funs, ...)
}

#' @importFrom dplyr summarise
#' @export
plot.multSim <- function(x, title, subtitle, ...){
  
  data <- getMultSimData(x)
  
  if(missing(title)) 
    title <- x$model@descr
  
  if(missing(subtitle)){
    subtitle <- paste0("Number of simulations: ", length(x$seeds), "\n")
    subtitle <- paste0(subtitle, format(x$model, 
                                        short = FALSE, 
                                        collapse = "\n",
                                        slots = c("init", "parms")))
  }
  plot(data, title = title, subtitle = subtitle, ...)
}
