#======== todo =================================================================

#' @include pdmp_class.R pdmp_sim.R multSim.R multSimData_outputMethods.R
NULL

#### output methods #####

#' @importFrom utils str
#' @importFrom stats median
#' @rawNamespace S3method(print, multSim)
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
        bool <- vapply(x$outputList, 
                       function(i) sum(class(i) %in% c("deSolve", "data.frame")) > 0, 
                       logical(1))
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
hist.multSim <- function(x, t, main, sub, ...){
  if(missing(main)) main <- x$model@descr
  if(missing(sub)) sub <- paste0("Histogram for t = ", t, "\n",
                                format(x$model, short = FALSE, slots = "parms"))
  data <- getMultSimData(x, times = t)
  hist(data, t = t, main = main, sub = sub, ...)
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

#' @rdname plotSeeds
#' @importFrom ggplot2 ggtitle
#' @export
plotSeeds.multSim <- function(x, seeds, ...){
  if(missing(seeds)) seeds <- x$seeds
  data <- getMultSimData(x, seeds = seeds)
  plot <- plotSeeds(data, seeds, ...) + 
    ggplot2::labs(title = x$model@descr,
                  subtitle = format(x$model, short = FALSE, collapse = "\n",
                                    slots = c("init", "parms")))
  return(plot)
}

#' @rdname plotTimes
#' @importFrom ggplot2 ggtitle
#' @export
plotTimes.multSim <- function(x, vars, times, nolo = 0, 
                              plottype = "boxplot", ...){
  if(missing(vars)) 
    vars <- names(x$model@init)
  if(missing(times)){
    t <- fromtoby(x$model@times)
    times <- t[seq(1, length(t), length.out = 11)]
  }
  
  data <- getMultSimData(x, times = times)
  plot <- plotTimes(data, times = times, vars = vars, 
                    nolo = nolo, plottype = plottype, ...) 
  
  subtitle <- paste0(plot$label$subtitle, "\n",
                     format(x$model, short = FALSE, collapse = "\n",
                            slots = c("init", "parms")))
  
   plot <- plot +  ggplot2::labs(title = x$model@descr, subtitle = subtitle)
  return(plot)
}

#' @rdname plotStats
#' @export
plotStats.multSim <- function(x, vars, funs, ...){
 if(missing(vars)) vars <- names(x$model@init)
 data <- getMultSimData(x)
 plot <- plotStats(data, vars, funs, ...) 
 
 subtitle <- paste0(plot$label$subtitle, "\n",
                    format(x$model, short = FALSE, collapse = "\n",
                           slots = c("init", "parms")))
 
 plot <- plot +  ggplot2::labs(title = x$model@descr, subtitle = subtitle)
 return(plot)
}

#' @rdname plot
#' @method plot multSim
#' @export
plot.multSim <- function(x, title, subtitle, ...){
  # x <- removeSeeds(x)
  data <- getMultSimData(x)
  
  if(missing(title)) 
    title <- x$model@descr
  
  plot <- plot(data, title = title, ...)
  
  if(missing(subtitle)){
    subtitle <- paste0(plot$label$subtitle, "\n",
                       format(x$model, short = FALSE, collapse = "\n",
                             slots = c("init", "parms")))
  }
  
  plot <- plot + ggplot2::labs(subtitle = subtitle)
  return(plot)
}
