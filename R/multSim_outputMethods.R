#======== todo =================================================================
# < 80 chars per line
# plot: stetige/diskrete Variablen über pdmp-slot bestimmen
# NA for den Plots ausführen
# plot: title und subtitle übergben können
# subtitle in plot: line = ... abhängig von Zeilenanzahl
# subtitle: anzahl der Seeds mit Hinschreiben
# plot: image statt image3D

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

plot.multSim <- function(x, nbins = 30, ...){
  if (!requireNamespace("plot3D", quietly = TRUE)) {
    stop("Pkg 'plot3D' needed for this function to work. 
          Please install it.", call. = FALSE)
  }
  
  #------ prepare data and plot settings -----
  
  n <- d <- NULL # n = index of continous vars, d = index of discrete variables
  for(i in seq_along(x$model@init)){
    if(length(unique(x$outputList[[1]][, i+1])) > 6) 
      n <- cbind(n, i+1) # determination of continous variables is only 
                         # based on first simulation
    else 
      d <- cbind(d, i+1)
  }
  # concatenate all simulations to one matrix (rows with NA are removed):
  values <- na.exclude(do.call("rbind", x$outputList)) 
  t_c <- cut(values[,1], nbins)
  t_sc <- seq(min(values[,1]), max(values[,1]), length.out = nbins)
  
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mar = c(3,3,1,3), oma = c(0,0,7,0))
  layout(matrix(c(rep(seq_along(n), each = length(d)), 
                  length(n) + seq_along(d) ), 
                ncol = length(n) + 1))
  
  #----- plot continous variables ------
  
  for(i in seq_along(n)){

    x_c <- cut(values[,n[i]], nbins)
    x_sc <- seq(min(values[,n[i]]), max(values[,n[i]]), length.out = nbins)
    freq <- table(t_c, x_c)

    plot3D::image2D(x = t_sc, y = x_sc, z = freq, ylab = "",
                    xlab = "time", main = colnames(values)[n[i]], ...)
    contour(x = t_sc, y = x_sc, z = freq, add = TRUE, col = "black") 
    #col = rgb(1,1,1,.7)
  }

  #---- plot discrete variables -----

  for(i in seq_along(d)){
    freq <- prop.table(table(values[,1], values[,d[i]]),1)
    par(mar = c(6,4,2,4), xpd = TRUE)
    matplot(x$outputList[[1]][,1], freq, type = "l", lty = 1, 
            col = rainbow(ncol(freq)), xlab = "time", ylab = "", 
            main = colnames(values)[d[i]], ...)
    legend("topleft", bty = "n", legend = colnames(freq), 
           col=rainbow(ncol(freq)), pch=15, cex = 1.0, horiz = TRUE)
  }
  
  #---- text ---------
  
  title <- x$model@descr
  subtitle <- format(x$model, short = FALSE, collapse = "\n", 
                     slots = c("init", "parms"))
  mtext(title, cex = 1.5, font = 2, line = 4, outer = TRUE)
  mtext(subtitle, line = -1, outer = TRUE)
}


gplot <- function(x, nbins = 30, ...){
  
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
  discrData <- summarise(discrData, count = n())
  discrData$value <- as.ordered(discrData$value)
  
  print(str(discrData))
  
  #----- plot ------
  
  plot <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = time))
  plot <- plot + ggplot2::geom_bin2d(data = contData, ggplot2::aes(y = value))
 # plot <- plot + ggplot2::geom_density_2d(data = contData, ggplot2::aes(y = value))
 # plot <- plot + ggplot2::stat_summary_2d(data = contData)
  
  #plot <- plot + ggplot2::geom_line(data = discrData, 
  #                                  ggplot2::aes(y = count, group = value, color = value))
  plot <- plot + ggplot2::geom_smooth(data = discrData, method = "auto",
                                    ggplot2::aes(y = count, group = value, color = value))
  plot <- plot + ggplot2::facet_wrap( ~ variable)
  
  return(plot)
 
  #---- text ---------
  
  title <- x$model@descr
  subtitle <- format(x$model, short = FALSE, collapse = "\n", 
                     slots = c("init", "parms"))
  mtext(title, cex = 1.5, font = 2, line = 4, outer = TRUE)
  mtext(subtitle, line = -1, outer = TRUE)
  }