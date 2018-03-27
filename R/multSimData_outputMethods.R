#' @include multSimData.R
NULL


#' @importFrom dplyr summarise
#' @export
plot.multSimData <- function(x, title = NULL, subtitle = NULL, 
                             discPlot = "smooth", contPlot = "bin2d", ...){
  require(ggplot2)
  
  ### prepare data
  contData <- subset(x, type == "cont")
  discData <- subset(x, type == "disc")
  discData$variable <- factor(discData$variable)
  discData <- group_by(discData, variable, time, value)
  discData <- dplyr::summarise(discData, count = n())
  discData$value <- as.ordered(discData$value)
  
  plot <- ggplot(data = NULL, aes(x = time))
  
  ### continous variables
  if(contPlot == "bin2d"){
    n <- length(unique(contData$variable))
    plot <- plot + geom_bin2d(data = contData, aes(y = value), ...) +
      #viridis::scale_fill_viridis() +
      scale_fill_distiller(palette = "Spectral", 
                           name = switch(as.character(length(n)),
                                         "1" = "Continous\nvariable",
                                         "Continous\nvariables"))
  }
  if(contPlot == "density_2d"){
    plot <- plot + stat_density_2d(data = contData,
                                   aes(y = value, colour = value),
                                   ...)
  }
  ### discrete variables
  if(discPlot == "line"){
    plot <- plot + geom_line(data = discData,
                             aes(y = count, group = value, color = value),
                             ...)
  }
  if(discPlot == "smooth"){
    plot <- plot + geom_smooth(data = discData, method = "auto",
                               aes(y = count, group = value, color = value),
                               ...)
  }
  plot <- plot + scale_color_discrete(name = "Discrete\nValues")
  
  ### text
  if(missing(subtitle))
    subtitle <- paste("Number of simulations:", length(unique(x$seed)))
  plot <- plot + labs(title = title, subtitle = subtitle, y = NULL)
  
  ### facet
  plot <- plot + facet_wrap( ~ variable, scales = "free_y")
  
  return(plot)
}

#' Plot method for simulations
#' 
#' This method plots the simulation for every seed in the input. 
#' Continous variables are plotted as line plots, 
#' discrete variables in a stacked barplot above.
#' The plot is created with \pkg{ggplot2} and can be modified afterwards.
#' 
#' @param x data.frame of class \code{\link{multSimData}}.
#' @note A maximal number of 4 seeds can be plotted. 
#' The method requires the package \pkg{tidyr}.
#' @export
plotSeeds <- function(x, trange = c(0, tail(x$time, 1)), log = FALSE){
  
  if(!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Pkg 'tidyr' needed for this function to work. 
         Please install it.", call. = FALSE)
  }
  if(!is.multSimData(x)){
    stop("Parameter x has to be of type 'multSimData'.")
  }
  if(length(unique(x$seed)) > 4){
    stop("To many seeds to plot. A maximum of 4 seeds can be plotted.")
  }
  
  #** CMD Check preparations
  #* The following assignments are made to avoid the R CMD Check NOTE
  #* no visible binding for global variable ......
  #* These notes occur because surv, lower, upper, and variable are called
  #* within transform(), melt(), and ddply() where there is a data argument
  #* that R CMD check can't reconcile with the variables.
  value <- variable <- NULL
  
  #------ Prepare the data for plotting ------------
  
  x$seed <- factor(x$seed)
  levels(x[, "seed"]) <- paste0("Seed = ", levels(x[,"seed"]))
  contData <- subset(x, type == "cont")
  discData <- subset(x, type == "disc")
  discData$variable <- factor(discData$variable)
  discVarNames <- sort(levels(discData$variable))
  discData <- tidyr::spread(discData, variable, value)
  
  # define colors for discrete variables
  color_mapping <- function(discVar, value){
    index <- which(discVar == discVarNames)
    allValues <- unique(discData[, discVar])
    levelindex <- which(value == allValues)
    rainbow(length(discVarNames), v = levelindex/length(allValues))[index]
  }
  for(name in discVarNames){
    discData[, paste0("col_", name)] <- vapply(discData[, name],
                                            function(v) color_mapping(name, v),
                                            character(1))
  }

  #---------- Create Plot ---------------------
  
  plot <- ggplot(data = NULL, aes(x = time)) + 
    coord_cartesian(xlim=trange) +   # zoom in
    labs(y = "", x = "time")  
  
  #** Plot discrete variables
  # if(log){
  #   plot <- plot +
  #     ggplot2::scale_y_log10() + 
  #     ggplot2::geom_rect(ggplot2::aes_string(xmin = "time", xmax = "time+1", 
  #                                            ymin = 0, ymax = Inf, 
  #                                            fill = discVarName)) +
  #     ggplot2::geom_hline(yintercept = 1, col = "grey")
  # }
  # else{
  #   plot <- plot +
  #     ggplot2::geom_rect(ggplot2::aes_string(xmin = "time", xmax = "time+1", 
  #                                            ymin = -Inf, ymax = Inf, 
  #                                            fill = discVarName))
  # }
  
  #** Plot discrete variables
  min <- min(contData$value)
  height <- abs(max(contData$value) - min)/20
  discValues <- NULL
  discCols <- NULL
  for(i in seq_along(discVarNames)){
    
     vals <- unique(discData[, discVarNames[i]])
     cols <- unique(discData[, paste0("col_", discVarNames[i])])
     discCols <- c(discCols, cols)
     names(vals) <- rep(discVarNames[i], length(vals))
     discValues <- c(discValues, vals)
     
     plot <- plot + geom_rect(data = discData,
       aes_string(
         fill = paste0("col_", discVarNames[i]), 
         xmin = "time", xmax = "time+1", 
         ymin = min - i*height, ymax = min - (i - 1)*height))
  }
  plot <- plot + scale_fill_identity("discrete\nvariables",
                                     guide = "legend",
                                     breaks = discCols,
                                     labels = printVect(discValues,
                                                        collapse = NULL)
  )

  #** Plot continous variables
  cols <- c("#009E73", "#0072B2", "#D55E00", "#E69F00", 
            "#56B4E9", "#CC79A7", "#F0E442")
  plot <- plot + 
    geom_line(data = contData, 
              aes(x = time, y = value, colour = variable), 
              size = 1) +
    scale_colour_manual(name = "continous\nvariables", 
                        values = cols[1:length(levels(contData$variable))])
  
  # facet_wrap
  plot <- plot + facet_wrap( ~ seed, ncol = 2)
  #plot <- plot + facet_grid(variable ~ seed)
  
  print(plot)
  invisible(plot)
  return(plot)
  }