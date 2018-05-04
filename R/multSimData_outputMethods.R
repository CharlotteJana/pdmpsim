#======== todo =================================================================
#t2 hist: bei mehreren diskreten Variablen wird der Barplot nicht 
#t2       korrekt beschriftet (names.arg = c("dA", "dB") geht nicht!)
#t2 plotStats: label oben hinzufügen
#t1 plotStats, plotTimes: missing funktioniert nicht, warum?
#t2 plotStats: Titel ermöglichen
#t1 summarize_at: correct generic function definition?
#t2 density: in plotDensity umbenennen?
#t3 density: warum muss stats in imports und darf nicht zu suggest?
#t3 hist und density für multSimCsv
#t1 docmuentation anpassen, nachdem ddomain slot eingefügt
#t2 plotSeeds: parameter seeds - warum nur wenn x multSim ist?
#t1 hist & density mit toggleSwitch zeigen nur eine diskrete Variable

#' @include multSimData.R
NULL

#========== plot ===============

#' @importFrom dplyr summarise
#' @importFrom ggplot2 ggplot aes geom_bin2d stat_density_2d geom_line
#' @importFrom ggplot2 geom_smooth scale_color_discrete labs facet_wrap
#' @export
plot.multSimData <- function(x, title = NULL, subtitle = NULL, 
                             discPlot = "smooth", contPlot = "bin2d", ...){

  ### prepare data
  
  # to avoid the R CMD Check NOTE 'no visible binding for global variable ...'
  time <- type <- variable <- value <- count <- NULL
  
  contData <- subset(x, type == "cont")
  discData <- subset(x, type == "disc")
  discData$variable <- factor(discData$variable)
  discData <- group_by(discData, variable, time, value)
  discData <- dplyr::summarise(discData, count = n())
  discData$value <- as.ordered(discData$value)
  
  plot <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = time))
  
  ### continous variables
  if(contPlot == "bin2d"){
    n <- length(unique(contData$variable))
    plot <- plot + ggplot2::geom_bin2d(data = contData, 
                                       ggplot2::aes(y = value), ...) +
      #viridis::scale_fill_viridis() +
      ggplot2::scale_fill_distiller(palette = "Spectral", 
                                    name = switch(as.character(length(n)),
                                                  "1" = "Continous\nvariable",
                                                  "Continous\nvariables"))
  }
  
  if(contPlot == "density_2d"){
    plot <- plot + ggplot2::stat_density_2d(
      data = contData, ggplot2::aes(y = value, colour = value),...)
  }
  ### discrete variables
  if(discPlot == "line"){
    plot <- plot + ggplot2::geom_line(
      data = discData, ggplot2::aes(y=count, group=value, color=value), ...)
  }
  if(discPlot == "smooth"){
    plot <- plot + ggplot2::geom_smooth(
      data = discData, ggplot2::aes(y = count, group = value, color = value),
      method = "auto", ...)
  }
  plot <- plot + ggplot2::scale_color_discrete(name = "Discrete\nValues")
  
  ### text
  if(missing(subtitle))
    subtitle <- paste("Number of simulations:", length(unique(x$seed)))
  plot <- plot + ggplot2::labs(title = title, subtitle = subtitle, y = NULL)
  
  ### facet
  plot <- plot + ggplot2::facet_wrap( ~ variable, scales = "free_y")
  
  return(plot)
}

#========== plotSeeds ===============

#' Plot method for simulations
#' 
#' This method plots the simulation for every seed in the input. 
#' Continous variables are plotted as line plots, 
#' discrete variables in a stacked barplot beneath.
#' The plot is created with \pkg{ggplot2} and can be modified afterwards.
#' 
#' @param x object of class \code{\link{multSim}} or \code{\link{multSimData}}.
#' @param ... additional arguments, currently not used.
#' @note A maximal number of 4 seeds can be plotted. 
#' The method requires the package \pkg{tidyr}.
#' @examples
#' data("toggleSwitch")
#' msim <- multSim(toggleSwitch, seeds = 1:2)
#' plotSeeds(msim)
#' plotSeeds(msim, seeds = 1) + ggplot2::facet_grid(variable ~ seed)
#' @importFrom utils tail
#' @importFrom grDevices rainbow
#' @importFrom ggplot2 ggplot aes labs geom_rect aes_string geom_line
#' @importFrom ggplot2 scale_fill_identity scale_color_manual facet_wrap
#' @export
plotSeeds <- function(x, ...){
  UseMethod("plotSeeds", x)
}

#' @rdname plotSeeds
#' @export
plotSeeds.multSimData <- function(x, ...){
  
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
  time <- value <- variable <- type <- NULL
  
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
  
  plot <- ggplot2::ggplot(data = NULL, ggplot2::aes(x = time)) + 
    ggplot2::coord_cartesian(xlim = c(0, tail(x$time, 1))) +   # zoom in
    ggplot2::labs(y = "", x = "time")  
  
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
     
     plot <- plot + ggplot2::geom_rect(data = discData,
                                       ggplot2::aes_string(
         fill = paste0("col_", discVarNames[i]), 
         xmin = "time", xmax = "time+1", 
         ymin = min - i*height, ymax = min - (i - 1)*height))
  }
  plot <- plot + ggplot2::scale_fill_identity(
    "discrete\nvariables", guide = "legend", breaks = discCols,
    labels = printVect(discValues,collapse = NULL)
  )

  #** Plot continous variables
  cols <- c("#009E73", "#0072B2", "#D55E00", "#E69F00", 
            "#56B4E9", "#CC79A7", "#F0E442")
  plot <- plot + 
    ggplot2::geom_line(data = contData, 
                       ggplot2::aes(x = time, y = value, colour = variable), 
                       size = 1) +
    ggplot2::scale_colour_manual(
      name = "continous\nvariables", 
      values = cols[seq_along(levels(contData$variable))])
  
  # facet_wrap
  plot <- plot + ggplot2::facet_wrap( ~ seed, ncol = 2)
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
#' \code{\link{getMultSimData}}. It basically groups the data before 
#' summarising, so that the functions are applied for every time value 
#' separately.
#' @param .tbl a data.frame returned by method \code{\link{getMultSimData}}
#' @param .vars a character vector of variable names to apply the functions for
#' @param .funs a list of function calls generated by \code{\link[dplyr]{funs}}, 
#' or a character vector of function names, or simply a function.
#' @param ... Additional arguments for the function calls in .funs.
#' @return a \code{tbl} with the applied function values
#' @examples 
#' data("simplePdmp")
#' md <- getMultSimData(multSim(simplePdmp, 1:20))
#' summarise_at(md, .vars = c("f", "d"), .funs = c("min", "max"))
#' summarise_at(md, .vars = "f", .funs = dplyr::funs(10 * quantile(., 0.75)))
#' @seealso \link[dplyr]{summarise_at} for the original method, 
#' \link{plotStats} to plot the calculated values
#' @export
summarise_at <- function (.tbl, .vars, .funs, ...) {
  UseMethod("summarise_at", .tbl)
}

#' @rdname summarise_at
#' @importFrom dplyr summarise_at group_by
#' @export
summarise_at.multSimData <- function(.tbl, .vars, .funs, ...){
  
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Pkg 'tidyr' needed for this function to work. 
         Please install it.", call. = FALSE)
  }
  
  # to avoid the R CMD Check NOTE 'no visible binding for global variable ...'
  time <- type <- variable <- value <- NULL
  
  .tbl <- subset(.tbl, select = -type)
  .tbl <- tidyr::spread(.tbl, variable, value)
  .tbl <- group_by(.tbl, time)
  return(dplyr::summarise_at(.tbl, .vars, .funs, ..., .cols = NULL))
}

#' Plot statistics of multiple simulations
#' 
#' This method plots descriptive statistics as i. e. mean, min, max, sd
#' over all simulations depending on the time. The desired statistic values
#' can be specified individually with the parameter \code{funs}.
#' 
#' @param x object of class \code{\link{multSim}} or \code{\link{multSimData}}.
#' @param vars character vector giving the names of variables to plot the 
#' statistics for.
#' @param funs a list of function calls generated by \code{\link[dplyr]{funs}}, 
#' or a character vector of function names, or simply a function.
#' @param ... Additional arguments for the function calls in funs.
#' @examples 
#' data("toggleSwitch")
#' ms <- multSim(toggleSwitch, seeds = 1:10)
#' plotStats(ms, vars = c("fA", "fB"), funs = "mean")
#' plotStats(ms, vars = "fB", funs = c("min", "max", "mean"))
#' @note The method requires the packages \pkg{grid} and \pkg{gtable}.
#' @seealso \link{summarise_at} to get the calculated values of the statistics
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line scale_x_continuous theme ggplotGrob
#' @importFrom ggplot2 unit geom_blank geom_segment arrow geom_text
#' @importFrom ggplot2 guides theme_minimal element_blank element_rect
#' @importFrom dplyr mutate if_else group_by funs
#' @export
plotStats <- function(x, vars, funs, ...){
  UseMethod("plotStats", x)
}

#' @rdname plotStats
#' @export
plotStats.multSimData <- function(x, vars, funs, ...){
  
  ##todo: missing funktioniert nicht, warum ????
  if(missing(vars)) vars <- levels(x$variable) # names of all variables
  if(missing(funs)) funs <- dplyr::funs("min", "median", "mean", "max", "sd")
  
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Pkg 'grid' needed for this function to work. 
         Please install it.", call. = FALSE)
  }
 
  if (!requireNamespace("gtable", quietly = TRUE)) {
    stop("Pkg 'gtable' needed for this function to work. 
         Please install it.", call. = FALSE)
  }
  
  #------ Prepare the data for plotting ------------

  data <- reshape2::melt(summarise_at(x, vars, funs, ...), id = c("time"))
  colnames(data)[2] <- "fun"
  
  # to avoid the R CMD Check NOTE 'no visible binding for global variable ...'
  value <- fun <- name <- type <- time <- NULL
  
  #---------- Create Plot ---------------------
  
  plot <- ggplot2::ggplot(data = data, ggplot2::aes(
    x = time, y = value, group = fun, color = fun)) +
    ggplot2::geom_line() + 
    ggplot2::scale_x_continuous(expand = c(0,0)) + # cut plot region at xmax
    ggplot2::theme(legend.position = "none", 
                   plot.margin = unit(c(1,0,1,1),"line"))
  
  #---------- Create labeling on the right side ---------
  
  d2 <- data[which(data$time == max(data$time)),]
  #d2 <- ddply(data, "fun", summarise, time=0, value=value[length(value)])
  
  plegend <- ggplot2::ggplot(data, ggplot2::aes(x=time, y=value, colour=fun)) + 
    ggplot2::geom_blank() +
    ggplot2::geom_segment(data = d2, 
                          ggplot2::aes(x=2, xend = 0, y = value, yend = value),
                          arrow = ggplot2::arrow(length=unit(2,"mm"), 
                                                 type="closed")) +
    ggplot2::geom_text(data = d2, ggplot2::aes(x=2.5, label = fun), hjust = 0) +
    ggplot2::scale_x_continuous(expand = c(0,0)) + # cut plot region at xmax
    ggplot2::guides(colour = "none") + 
    ggplot2::theme_minimal() + 
    ggplot2::theme(line = ggplot2::element_blank(),
                    text = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill="grey95", 
                                                            linetype = "blank"))
  gl <- gtable::gtable_filter(ggplotGrob(plegend), "panel")
  
  # add a cell next to the main plot panel, and insert gl there
  g <- ggplot2::ggplotGrob(plot)
  index <- subset(g$layout, name == "panel")
  g <- gtable::gtable_add_cols(g, ggplot2::unit(1, "strwidth", "line # 1") + 
                                  ggplot2::unit(1, "cm"))
  g <- gtable::gtable_add_grob(g, gl, t = index$t, 
                               l = ncol(g), b = index$b, r = ncol(g))
  #grid::grid.newpage()
  grid::grid.draw(g)
  
  #print(g)
  #invisible(g)
  #return(g) 
}

#========== plotTimes ===============

#' Plot at different times
#' 
#' Plot a boxplot or violin plot or dotplot for every time value 
#' in the data.frame (a maximum of 12 different time values is allowed).
#' All outliers that outreach a given threshold will be plotted with a
#' number on their side. This number represents the seed number that was
#' used to simulate the simulation that causes the outlier.
#' A red diamond represents the median of the simulated values.
#' The plot is created with \pkg{ggplot2} and can be modified afterwards.
#' @param x object of class \code{\link{multSim}} or \code{\link{multSimData}}.
#' @param vars character vector of variable names that shall be plotted
#' @param times numeric vector with time values to plot. If no vector is given,
#' ten values between the minimum and maximum provided time values will be 
#' plotted.
#' @param threshold a positive number. Seed numbers will be printed aside
#' every outlier whose absolute value is greater than threshold.
#' @param plottype character vector determining the type of the plot.
#' Possible values are 'boxplot', 'violin' or 'dotplot'. Defaults to 'boxplot'.
#' @param ... additional parameters for the plotting function (either 
#' geom_boxplot, geom_violin or geom_dotplot, depending on parameter plottype).
#' @examples 
#' data("simplePdmp")
#' times(simplePdmp) <- c(from = 0, to = 5, by = 1)
#' md <- getMultSimData(multSim(simplePdmp, 1:8), times = 1:5)
#' plotTimes(md, plottype = 'violin')
#' plotTimes(md, threshold = 1)
#' @importFrom dplyr mutate
#' @export
plotTimes <- function(x, vars, times, threshold=NULL, plottype="boxplot", ...){
  UseMethod("plotTimes", x)
}
  
#' @rdname plotTimes
#' @export
plotTimes.multSimData <- function(x, vars, times, threshold = NULL, 
                                  plottype = "boxplot", ...){
  
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Pkg 'ggrepel' needed for this function to work. 
         Please install it.", call. = FALSE)
  }

  if(missing(vars)) vars <- levels(x$variable) # names of all variables
  if(missing(times)){
    t <- unique(x$time)
    times <- t[seq(1, length(t), 10)]
  }
    x <- getMultSimData(x, times = times)
  
  if(length(unique(x$time)) > 12)
    stop("To many different values for variable \"time\".")
  
  #------ Prepare the data for plotting ------------
  
  x$time <- as.factor(x$time)
  levels(x$time) <- paste0("t = ", levels(x$time))
  
  data <- subset(x, select = -type)
  data <- data[data$variable %in% vars, ]
  
  # to avoid the R CMD Check NOTE 'no visible binding for global variable ...'
  variable <- value <- print.outlier <- type <- NULL
  
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
    plot <- plot + ggplot2::geom_boxplot(...)
  else if(plottype == "violin")
    plot <- plot + ggplot2::geom_violin(draw_quantiles = 0.5, ...)
  else if(plottype == "dotplot")
    plot <- plot + ggplot2::geom_dotplot(binaxis="y", stackdir="center", ...)
  else
    stop("variable 'plottype' should be either 'boxplot', 
         'violin' or 'dotplot'.")
  
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
  
  plot <- plot + ggplot2::facet_grid(variable ~ time) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())
  
  print(plot)
  invisible(plot)
  return(plot)
  }

#========== hist ================

#' Histogram over all simulations
#' 
#' A plot method for simulations of a piecewise deterministic markov process 
#' (\code{\link{PDMP}}). It plots every continous variable in its own histogram 
#' and all discrete variables in a stacked barplot. 
#' 
#' @note All variables with more than six different values are considered as 
#' continous.
#' @param x object of class \code{\link{multSimData}} or \code{\link{multSim}}.
#' @param t a single time value at which the histogram shall be plotted.
#' The parameter can be omitted if \code{x} contains only one time value.
#' @param main optional character string for the title of the plot. If \code{x} 
#' is a \code{multSim} object, \code{main} will be set as \code{descr(x$model)} 
#' if not otherwise specified.
#' @param sub optional character string for the subtitle of the plot. The 
#' default value for a \code{multSim} object \code{x} gives informations about 
#' parameters and the initial values.
#' @param ... additional parameters passed to the default method of 
#' \code{\link[graphics]{hist}}
#' @name hist
#' @examples 
#' data("simplePdmp")
#' ms <- multSim(simplePdmp, seeds = 1:10)
#' hist(ms, t = 10)
#' hist(getMultSimData(ms), t = 10, density = 10)
#' @importFrom graphics hist par plot.new layout barplot text mtext
#' @importFrom grDevices dev.list dev.off gray.colors
#' @export
hist.multSimData <- function(x, t, main, sub, ...){
  
  if(missing(t)) t <- unique(x$time)
  if(length(t) > 1) 
    stop("This method requires a single value for variable 't'.")
  
  #------ Prepare the data for plotting ------------
  
  # to avoid the R CMD Check NOTE 'no visible binding for global variable ...'
  time <- type <- variable <- NULL
  
  data <- subset(x, time == t)
  if(nrow(data) == 0) 
    stop("There are no simulations for t = ", t, ".")
  
  contData <- subset(data, type == "cont")
  discData <- subset(data, type == "disc")
  n <- unique(contData$variable)
  d <- unique(discData$variable)
  
  #---------- Create Plot ---------------------
  
  if(!is.null(grDevices::dev.list())) grDevices::dev.off()
  graphics::plot.new()
  opar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(opar))
  graphics::par(oma = c(0,1,4,0)) #mfrow = c(1,n+1))
  graphics::layout(t(c(rep(seq_along(n), each = 2), length(n)+1)))
  
  for(name in n){
    graphics::hist(subset(contData, variable == name)$value,
                   xlab = name, ylab = "", col = "grey", main = NULL, ...)
  }
  
  dVal <- NULL
  dRange <- unique(discData$value)
  for(name in d){
    dVal <- cbind(dVal, as.matrix(table(
      c(subset(discData, variable==name)$value, dRange) - 1
    )))
  }
  colnames(dVal) <- d
  # warum funktioniert names.arg = as.character(d) nicht????
  b <- graphics::barplot(dVal, beside = FALSE, axes = FALSE, 
                         col = grDevices::gray.colors(nrow(dVal), start = 0.6))
  
  #text for the bars
  h <- vapply(seq_len(ncol(dVal)), 
              function(i) cumsum(dVal[, i]),
              numeric(nrow(dVal)))-dVal/2
  smallBarIndex <- which(dVal <= 0.04*nrow(discData), arr.ind = TRUE)
  if(length(smallBarIndex) != 0){
    for(i in seq_len(nrow(smallBarIndex))){
      h[smallBarIndex[i,1], smallBarIndex[i,2]] <- NA
    }
  }
  graphics::text(b, y = t(h), 
                 labels = rep(levels(factor(dRange)), each = length(d)))
  
  # title
  if(missing(main)) main <- NULL
  if(missing(sub)) sub <- paste("Histogram of ", length(unique(data$seed)), 
                                " simulations at time t = ", t, ".", sep = "")
  if(!is.null(main)) graphics::mtext(main, font = 2, line = 0, 
                                     cex = 1.5, outer = TRUE)
  if(!is.null(sub)) graphics::mtext(sub, line = -2, outer = TRUE)
}

#========== density ================

#' @title Plot Density Estimations
#'  
#' @description A plot method for simulations of a piecewise deterministic 
#' markov process (\code{\link{PDMP}}). It computes and plots a density over 
#' all simulations of the PDMP, seperately for every time value and every 
#' contious variable. Discrete variables are plotted in a stacked barplot.
#'
#' @param t a vector of time values at which the densities shall be plotted. 
#' If no vector is provided, every time value provided will
#' be plotted.
#' @param ... additional parameters passed to the default method of 
#' \code{\link[stats]{density}}
#' @inheritParams hist
#' @name density
#' @examples 
#' data("simplePdmp")
#' ms <- multSim(simplePdmp, seeds = 1:10)
#' density(ms, t = c(5, 10))
#' @importFrom stats density
#' @importFrom grDevices dev.list dev.off gray.colors colorRampPalette
#' @importFrom graphics plot.new par layout plot lines legend barplot text mtext
#' @export
density.multSimData <- function(x, t, main, sub, ...){ 
  
  # to avoid the R CMD Check NOTE 'no visible binding for global variable ...'
  time <- type <- variable <- NULL
  
  if(missing(t))
    t <- unique(x$time)
  if(length(t) > 20)
    stop("To many time values to plot. There should be less than
         20 different time values in the given data.")
  
  data <- subset(x, subset = time %in% t)
  if(nrow(data) == 0) 
    stop("There are no simulations for t = ", t, ".")
  
  contData <- subset(data, type == "cont")
  discData <- subset(data, type == "disc")
  n <- unique(contData$variable)
  d <- unique(discData$variable)
  simnr <- length(unique(x$seed))
  
  if(!is.null(grDevices::dev.list())) grDevices::dev.off()
  plot.new()
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(oma = c(0,1,4,0))
  layout(t(c(rep(seq_along(n), each = 2), seq_along(d) + length(n))))
  cols <- grDevices::colorRampPalette(c("green", "blue", "red"))(length(t))
  
  #density plot for every continuous variable
  for(name in n){
    dens <- lapply(t, 
              function(j) stats::density(
                subset(contData, time == j & variable == name)$value, 
                ...)
            )
    plot(NA, main = "",
         xlim = range(sapply(dens, "[", "x")), 
         ylim = range(sapply(dens, "[", "y")),
         xlab = name, ylab = "Density")
    mapply(lines, dens, col = cols)
    
  }
  if(length(t) != 1) 
    legend("topright", bty="n", legend=paste("t =", t), fill=cols, cex = 1.0)
  
  #barplot for discrete variable
  for(name in d){
    specVals <- sapply(lapply(t, 
                 function(j) subset(discData, time==j & variable==name)$value), 
                 cbind)
    discRange <- unique(c(specVals))
    discVal <- sapply(seq_along(t), function(m) 
      as.matrix(table(c(specVals[,m], discRange))) - rep(1, length(discRange)))

    # bars with colors as background
    fullbars <- rep(simnr, length(cols))
    barplot(fullbars, col = cols, axes = FALSE)
    
    b <- barplot(discVal, beside = FALSE, xlab = name, add = TRUE, axes = FALSE,
                 col = gray.colors(nrow(discVal), alpha = 0.6, end = 1))
    
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
    sub <- paste0("Density plot of ", simnr, " simulations ", timeText,".")
  } 
  if(!is.null(main)) mtext(main, font = 2, line = 0, cex = 1.5, outer = TRUE)
  if(!is.null(sub)) mtext(sub, line = -2, outer = TRUE)
}
