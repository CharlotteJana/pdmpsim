#======== todo =================================================================
# < 80 chars per line
# plot: stetige/diskrete Variablen über pdmp-slot bestimmen
# NA for den Plots ausführen
# plot: title und subtitle übergben können
# subtitle: anzahl der Seeds mit Hinschreiben

#' @include pdmp_class.R pdmp_sim.R multSim.R getTimeslice.R
NULL


#### output methods #####

print.multSim <- function(x){
  cat("An S3-object of class", class(x)[1], "with elements \n\n")
  names <- names(x)
  for (name in names) {
    content <- x[name]
    if (!is.null(content)) {
      if (name == "seeds") {cat("$seeds\n"); str(x$seeds)}
      if (name == "timeList") {
        times <- vapply(x$timeList, function(i) i[1:3], numeric(3))
        if(is.null(x$timeList)) cat("$timeList\nNULL\n")
        else{
          cat("$timeList\n\n time per simulation (median):\n")
          print(apply(times, 1, median, na.rm = TRUE))
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

summary.multSim <- function(x, entries = 1:(min(5,length(x$seeds)))){
  for(i in entries){
    cat("\nseed =", x$seeds[i],":\n\n")
    print(x$timeList[[i]])
    cat("\n")
    print(summary(x$outputList[[i]]))
  }
  if(length(entries) != length(x$seeds)){
    cat("\nOnly", length(entries) ,"out of", length(x$seeds), 
        "simulations are shown.")
  }
}

#' @rdname hist
hist.multSim <- function(x, t, main, ...){
  if(missing(main)) main <- x$model@descr
  timeSlice <- getTimeslice(x, times = t)
  hist(timeSlice, t = t, main = main, ...)
}

#' @param t a vector of time values at which the densities shall be plotted. 
#' If \code{x} is of class \code{\link{timeData}}, all existing time values 
#' will be taken.
#' @rdname density
density.multSim <- function(x, t, main, ...){
  if(missing(main)) main <- x$model@descr
  timeText <- ifelse(length(t) == 1, 
                     paste("at time t =", t), 
                     "at different times")
  timeSlice <- getTimeslice(x, times = t)
  density(timeSlice, main = main, ...)
}

#' @importFrom dplyr summarise
#' @export
plot.multSim <- function(x, title, subtitle, ...){
  
  require(ggplot2)
  
  #------ prepare data -----
  
  n <- d <- NULL # n = index of continous vars, d = index of discrete variables
  for(i in seq_along(x$model@init)){
    if(length(unique(x$outputList[[1]][, i+1])) > 6) 
      n <- cbind(n, i) # determination of continous variables is only 
    # based on first simulation
    else 
      d <- cbind(d, i)
  }
  discVarNames <- names(init(x$model))[d]
    
  # concatenate all simulations to one matrix (rows with NA are removed):
  data <- as.data.frame(na.exclude(do.call("rbind", x$outputList)))
  data <- reshape2::melt(data, id = c("time"))
  data$type <- vapply(data$variable, 
                      function(v) ifelse(v %in% discVarNames, "discr", "cont"),
                      character(1))
  
  contData <- subset(data, type == "cont")
  
  discrData <- subset(data, type == "discr")
  discrData$variable <- factor(discrData$variable)
  discrData <- group_by(discrData, variable, time, value)
  discrData <- dplyr::summarise(discrData, count = n())
  discrData$value <- as.ordered(discrData$value)
  
  #----- plot ------
  
  plot <- ggplot(data = NULL, aes(x = time))
  
  # continous variables
  contPlot <- "bin2d" # "bin2d" or "density_2d"
  if(contPlot == "bin2d"){
  plot <- plot + geom_bin2d(data = contData, aes(y = value)) +
          #viridis::scale_fill_viridis() +
          scale_fill_distiller(palette = "Spectral", 
                               name = switch(as.character(length(n)),
                                             "1" = "Continous\nvariable",
                                             "Continous\nvariables"))
  }
  if(contPlot == "density_2d"){
  plot <- plot + stat_density_2d(data = contData,
                                 aes(y = value, 
                                     colour = value))
  }
  
  # discrete variables
  discrPlot <- "smooth" # or "line"
  if(discrPlot == "line"){
  plot <- plot + geom_line(data = discrData, 
                           aes(y = count, group = value, color = value))
  }
  if(discrPlot == "smooth"){
  plot <- plot + geom_smooth(data = discrData, method = "auto",
                             aes(y = count, group = value, color = value))
  }
  plot <- plot + scale_color_discrete(name = "Discrete\nValues")
  
  # text
  if(missing(title)) 
    title <- x$model@descr
  if(missing(subtitle))
    subtitle <- format(x$model, short = FALSE, collapse = "\n",
                      slots = c("init", "parms"))
  plot <- plot + labs(title = title,
                      subtitle = subtitle,
                      ylab = "")
  
  # facet
  plot <- plot + facet_wrap( ~ variable, scales = "free_y")
  
  return(plot)
}