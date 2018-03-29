#' @include multSimData.R
NULL

#========== plot ===============

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

#========== plotSeeds ===============

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

#========== plotStats ===============

#' Summarise multSimData
#' 
#' This method overrides the default \code{\link{summarise_at}} method from 
#' package \pkg{dplyr} to work with data generated by function 
#' \code{\link{getMultSimData}}. It basically groups the data before summarising, 
#' so that the functions are applied for every time value separately.
#' @param .tbl a data.frame returned by method \code{\link{getMultSimData}}
#' @inheritParams dplyr::summarise_at
#' @export
summarise_at <- function (.tbl, .vars, .funs, ...) {
  UseMethod("summarise_at", .tbl)
}

#' @rdname summarise_at
#' @importFrom dplyr summarise_at group_by
#' @export
summarise_at.multSimData <- function(.tbl, .vars, .funs, ...){
  .tbl <- group_by(.tbl, time)
  #return( callGeneric(.tbl, .vars, .funs, ..., .cols = NULL))
  return(dplyr::summarise_at(.tbl, .vars, .funs, ..., .cols = NULL))
}

#' @importFrom dplyr mutate if_else group_by funs
plotStats <- function(x, vars, funs){
  
  ##todo: missing funktioniert nicht, warum ????
  if(missing(vars)) vars <- levels(x$variable) # names of all variables
  if(missing(funs)) funs <- dplyr::funs(min, median, mean, max, sd)
  
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Pkg 'grid' needed for this function to work. 
         Please install it.", call. = FALSE)
  }
  if (!requireNamespace("gtable", quietly = TRUE)) {
    stop("Pkg 'gtable' needed for this function to work. 
         Please install it.", call. = FALSE)
  }
  
  #------ Prepare the data for plotting ------------
  
  data <- subset(x, select = -type)
  data <- tidyr::spread(data, variable, value)
  data <- reshape2::melt(summarise_at(data, vars, funs), id = c("time"))
  colnames(data)[2] <- "fun"
  
  # to avoid the R CMD Check NOTE 'no visible binding for global variable ...'
  value <- fun <- name <- NULL
  
  #---------- Create Plot ---------------------
  
  plot <- ggplot(data = data, aes(
    x = time, y = value, group = fun, color = fun)) +
    geom_line() + 
    scale_x_continuous(expand = c(0,0)) + # cut plot region at xmax
    theme(legend.position = "none", plot.margin = unit(c(1,0,1,1),"line"))
  
  #---------- Create labeling on the right side ---------
  
  d2 <- data[which(data$time == max(data$time)),]
  #d2 <- ddply(data, "fun", summarise, time=0, value=value[length(value)])
  
  plegend <- ggplot(data, aes(x=time, y=value, colour=fun)) + 
    geom_blank() +
    geom_segment(data = d2, 
                 aes(x=2, xend = 0, y = value, yend = value),
                 arrow=arrow(length=unit(2,"mm"), type="closed")) +
    geom_text(data = d2, ggplot2::aes(x=2.5, label = fun), hjust = 0) +
    scale_x_continuous(expand = c(0,0)) + # cut plot region at xmax
    guides(colour = "none") + 
    theme_minimal() + 
    theme(line = element_blank(),
          text = element_blank(),
          panel.background = element_rect(
          fill="grey95", linetype = "blank"))
  gl <- gtable::gtable_filter(ggplotGrob(plegend), "panel")
  
  # add a cell next to the main plot panel, and insert gl there
  g <- ggplotGrob(plot)
  index <- subset(g$layout, name == "panel")
  g <- gtable::gtable_add_cols(g, unit(1, "strwidth", "line # 1") + 
                                 unit(1, "cm"))
  g <- gtable::gtable_add_grob(g, gl, t = index$t, 
                               l = ncol(g), b = index$b, r = ncol(g))
  grid::grid.newpage()
  grid::grid.draw(g)
  
  #print(g)
  #invisible(g)
  #return(g) 
}

#========== plotTimes ===============

#' @importFrom dplyr mutate
plotTimes <- function(x, vars, threshold = NULL, plottype = "boxplot"){
  
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

  if(missing(vars)) vars <- levels(x$variable) # names of all variables
  
  if(length(unique(x$time)) > 12)
    stop("To many different values for variable \"time\".")
  
  #------ Prepare the data for plotting ------------
  
  x$time <- as.factor(x$time)
  levels(x$time) <- paste0("t = ", levels(x$time))
  
  data <- subset(x, select = -type)
  data <- data[data$variable %in% vars, ]
  
  # to avoid the R CMD Check NOTE 'no visible binding for global variable ...'
  variable <- value <- print.outlier <- NULL
  
  #----- Find seeds that belong to outliers --------
  
  if(is.null(threshold)) threshold <- max(abs(data$value))
  
  print_outlier <- function(x) {
    return(x < -abs(threshold) | x > abs(threshold))
  }
  data <- mutate(data, print.outlier = ifelse(print_outlier(data$value), 
                                              data$seed, as.numeric(NA)))
  
  #---------- Create Plot ---------------------
  
  plot <- ggplot2::ggplot(data = data, ggplot2::aes(x = variable, y = value)) +
    ggplot2::labs(y = "", x = "") +
    ggplot2::geom_hline(yintercept = 1, col = "grey")
  
  if(plottype == "boxplot")
    plot <- plot + ggplot2::geom_boxplot()
  else if(plottype == "violin")
    plot <- plot + ggplot2::geom_violin(draw_quantiles = 0.5)
  else
    stop("variable 'plottype' should be either 'boxplot' or 'violin'.")
  
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
