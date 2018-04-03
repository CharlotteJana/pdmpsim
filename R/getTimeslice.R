#======== todo =================================================================
# rdname getData ändern
# getTimeslice: angeben, für welche Methoden das verwendet wird.
# immer überprüfen, ob es sich um timeData handelt
# plotStats: label oben hinzufügen
# plotStats: missing funktioniert nicht, warum?
# summarize_at: correct generic function definition?
# datei aufteilen
# density in plotDensity umbenennen?
# stetige/diskrete Variablen über pdmp-slot bestimmen
# density: warum muss stats in imports und darf nicht zu suggest?

#' @include pdmp_class.R pdmp_sim.R multSim.R msCsv.R
NULL

#----------- getTimeslice --------------

#' @rdname getData
#' @aliases getTimeslice gettimeslice
#' @export
getTimeslice <- function (x, times)  {
  UseMethod("getTimeslice", x)
}

#' @rdname getData
#' @importFrom dplyr select everything
#' @export
getTimeslice.multSim <- function(x, times){
  data <- NULL
  all.times <- do.call(seq, as.list(x$model@times))
  index <- NULL
  for(i in times){
    a <- which(all.times == i)
    if(length(a) == 0){
      warning("There are no simulations for time ", i)
    }
    else index <- c(index, a)
  }
  for(i in seq_along(index)){
    for(j in seq_along(x$outputList))
      data <- rbind(data, c(seed = x$seeds[j], x$outputList[[j]][index[i], ]))
  }
  data <- select(as.data.frame(data), time, everything()) #first column = "time"
  attr(data, "class") <- c("timeData", class(data))
  return(data)
}

#' @rdname getData
#' @export
getTimeslice.multSimCsv <- function(x, times){
  data <- NULL
  seeds <- x$seeds
  all.times <- fromtoby(x$model@times)
  index <- NULL
  
  # select times
  for(i in times){
    a <- which(all.times == i)
    if(length(a) == 0){
      warning("There are no simulations for time ", i)
    }
    else index <- c(index, a)
  }
  
  for(i in index){
    d <- data.frame(time = rep(all.times[i], length(seeds)), seed = seeds)
    for(j in seq_along(x$lafList)){
      d <- cbind(d, x$lafList[[j]][ , i+1])
    }
    colnames(d) <- c(colnames(d)[1:2], names(x$model@init))
    data <- rbind(data, d)
  }
  attr(data, "class") <- c("timeData", class(data))
  return(data)
}



#' @importFrom dplyr mutate
plotTimes_old <- function(tdata, vars, threshold = NULL){
  
  # tdata = data.frame with columns "seed", "time", 
  # and columns for the variables (named by the name of the variable)
  # method "getTimeslice" provides tdata in the required form for 
  # objects of class multSimCsv
  
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("Pkg 'reshape2' needed for this function to work. 
    Please install it.", call. = FALSE)
  }
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Pkg 'ggrepel' needed for this function to work. 
    Please install it.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Pkg 'ggplot2' needed for this function to work. 
    Please install it.", call. = FALSE)
  }
  
  if(missing(vars)) vars <- colnames(tdata)[-c(1, 2)] # names of all variables
  
  #------ Prepare the data for plotting ------------
  
  tdata[, "time"] <- as.factor(tdata[, "time"])
  levels(tdata[, "time"]) <- paste0("t = ", levels(tdata[,"time"]))
  if(length(levels(tdata[, "time"])) > 12) 
    stop("To many different values for variable \"time\".")
  
  tdata <- reshape2::melt(tdata, id = c("time", "seed"))
  tdata <- tdata[tdata$variable %in% vars, ]
  
  str(tdata)
  # to avoid the R CMD Check NOTE 'no visible binding for global variable ...'
  variable <- value <- print.outlier <- NULL
  
  #----- Find seeds that belong to outliers --------
  
  if(is.null(threshold)) threshold <- max(abs(tdata$value))
  
  print_outlier <- function(x) {
    return(x < -abs(threshold) | x > abs(threshold))
  }
  tdata <- mutate(tdata, print.outlier = ifelse(print_outlier(tdata$value), 
                                                tdata$seed, as.numeric(NA)))
  
  #---------- Create Plot ---------------------
  
  plot <- ggplot2::ggplot(data = tdata, ggplot2::aes(x = variable, y = value)) +
    ggplot2::labs(y = "", x = "") +
    ggplot2::geom_hline(yintercept = 1, col = "grey") +
    #ggplot2::geom_violin(draw_quantiles = 0.5) +
    ggplot2::geom_boxplot()
  
  # print red point for mean
  plot <- plot + ggplot2::stat_summary(fun.y = mean, geom = "point", 
                                       shape = 23, size = 3, fill = "red")
  # print seed numbers for outliers > threshold
  plot <- plot + ggrepel::geom_text_repel(ggplot2::aes(label = print.outlier), 
                                          na.rm = TRUE, nudge_x = 0.3, 
                                          segment.size = 0) 
  # logarithmic scale
  # plot <- plot + ggplot2::labs(y = "logarithmic scale") + 
  #                ggplot2::scale_y_log10()
  
  plot <- plot + ggplot2::facet_grid( ~ time)
  
  print(plot)
  invisible(plot)
  return(plot)
}



hist_old <- function(x, t, main, sub, ...){
  
  if(missing(main)) main <- NULL
  if(missing(sub)) sub <- paste("Histogram of ", nrow(x), 
                                " simulations at time t = ", t, ".", sep = "")
  
  n <- d <- NULL # n = index of continous vars, d = index of discrete vars
  for(i in 3:ncol(x)){
    if(length(unique(x[, i])) > 6) n <- cbind(n, i) 
    else d <- cbind(d, i)
  }
  values <- subset(x, time == t)
  
  if(length(t) > 1) 
    stop("This method requires a single value for variable 't'.")
  if(nrow(values) == 0) 
    stop("There are no simulations for t = ", t, ".")
  
  if(!is.null(dev.list())) dev.off()
  plot.new()
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(oma = c(0,1,4,0)) #mfrow = c(1,n+1))
  layout(t(c(rep(seq_along(n), each = 2), length(n)+1))) #1:length(d)+length(n)
  
  # histogram for every continuous variable
  for(i in n){
    hist(values[,i], xlab = colnames(values)[i], ylab = "", 
         col = "grey", main = NULL, ...)
  }
  
  discVal <- NULL
  discRange <- unique(unlist(x[, d]))
  for(i in seq_along(d)){
    discVal <- cbind(discVal, as.matrix(table(
                     c(values[, d[i]], discRange)) - rep(1, length(discRange))))
  }
  b <- barplot(discVal, beside = FALSE, names.arg = colnames(values)[d], 
               axes = FALSE, col = gray.colors(nrow(discVal), start = 0.6))
  
  #text for the bars
  h <- vapply(seq_len(ncol(discVal)), 
              function(i) cumsum(discVal[, i]),
              numeric(nrow(discVal)))-discVal/2
  smallBarIndex <- which(discVal <= 0.1*nrow(x), arr.ind = TRUE)
  if(length(smallBarIndex) != 0){
    for(i in seq_len(nrow(smallBarIndex))){
      h[smallBarIndex[i,1], smallBarIndex[i,2]] <- NA
    }
  }
  print(discVal)
  print(h)
  text(b, y=t(h), labels = rep(levels(factor(discRange)), each = length(d)))
  
  # title
  if(!is.null(main)) mtext(main, font = 2, line = 0, cex = 1.5, outer = TRUE)
  if(!is.null(sub)) mtext(sub, line = -2, outer = TRUE)
}

density_old <- function(x, main, sub, ...){ 
  
  #if (!requireNamespace("stats", quietly = TRUE)) {
  #  stop("Pkg 'stats' needed for this function to work. 
  #  Please install it.", call. = FALSE)
  #}
  
  t <- unique(x$time)
  n <- d <- NULL # n = index of continous vars, d = index of discrete variables
  for(i in 3:ncol(x)){
    if(length(unique(x[, i])) > 6) n <- cbind(n, i) 
    else d <- cbind(d, i)
  }
  values <- subset(x, subset = time %in% t)
  if(nrow(values) == 0) stop("There are no simulations for t = ", t, ".")
  
  if(!is.null(dev.list())) dev.off()
  plot.new()
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(oma = c(0,1,4,0))
  layout(t(c(rep(seq_along(n), each = 2), seq_along(d) + length(n))))
  cols <- colorRampPalette(c("green", "blue", "red"))(length(t))
  transCols <- paste0(cols, "6f")
 
  #density plot for every continuous variable
  for(i in n){
    dens <- lapply(t, 
                   function(j) stats::density(subset(x, time == j)[, i], ...))
    plot(NA, main = "",
         xlim = range(sapply(dens, "[", "x")), 
         ylim = range(sapply(dens, "[", "y")),
         xlab = colnames(x)[i], ylab = "Density")
    mapply(lines, dens, col=cols)
    
  }
  if(length(t) != 1) 
    legend("topright", bty="n", legend=paste("t =", t), fill=cols, cex = 1.0)
  
  #barplot for discrete variable
  for(i in d){
    specVals <- sapply(lapply(t, function(j) subset(x, time == j)[, i]), cbind)
    discRange <- unique(c(specVals))
    discVal <- sapply(seq_along(t), function(m) 
      as.matrix(table(c(specVals[,m], discRange))) - rep(1, length(discRange)))
    b <- barplot(discVal, beside = FALSE, xlab = colnames(values)[i],
                 axes = FALSE, col = gray.colors(nrow(discVal), start = 0.3))
    
    for(i in seq_along(cols)){ # each bar gets its own color
      disc <- discVal
      disc[,-i] <- NA
      colnames(disc)[-i] <- NA
      barplot(disc, col = c(rep(transCols[i], (nrow(discVal)))), 
              add = TRUE, axes = FALSE)
    }
    
    #text for the bars
    h <- sapply(seq_len(ncol(discVal)), 
                function(i) cumsum(discVal[,i])) - discVal/2
    text(b, y=t(h), labels = rep(levels(factor(discRange)), each = length(t)))
  }
  
  # title
  if(missing(main)) main <- NULL
  if(missing(sub)){
    if(length(t) == 1) timeText <- paste("at time t =", t) 
    else timeText <- "at different times"
    sub <- paste0("Density plot of ", length(unique(x$seed)), 
                  " simulations ", timeText,".")
  } 
  if(!is.null(main)) mtext(main, font = 2, line = 0, cex = 1.5, outer = TRUE)
  if(!is.null(sub)) mtext(sub, line = -2, outer = TRUE)
}
