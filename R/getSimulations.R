#' #======== todo =================================================================
#' # rdname getData ändern
#' # getSimulation: angeben, für welche Methoden das verwendet wird.
#' # getSimulation: prameter "seed" oder "seeds"? sollte konsistent sein!
#' # getSimulation für pdmpModel
#' # plotSeeds: legende bei diskreten variablen muss stimmen (ordnung der farben!)
#' 
#' #' @include pdmp_class.R pdmp_sim.R multSim.R msCsv.R
#' NULL
#' 
#' #----------- documentation ------------
#' 
#' #' Create data.frame with simulations
#' #'
#' #' The methods \code{getSimulation} and \code{getTimeslice} return a data.frame 
#' #' which gives for every row the seed, the simulation time and the simulated 
#' #' values at this time. This data.frame can be used as input for different plot 
#' #' methods, like ... .
#' #' @param x an object of class \code{\link{multSim}} or \code{\link{multSimCsv}}
#' #' @param seeds integer vector with specific seeds for which the simulation 
#' #' results (for all times) shall appear in the data.frame
#' #' @param times vector with specific time values for which the simulation 
#' #' results (for all seeds) shall appear in the data.frame
#' #' @return a data.frame with simulation results
#' #' @name getData
#' NULL
#' 
#' #----------- getSimulation --------------
#' 
#' #' @rdname getData
#' #' @aliases getSimulation getsimulation
#' #' @export
#' getSimulations <- function (x, seeds)  {
#'   UseMethod("getSimulations", x)
#' }
#' 
#' #' @rdname getData
#' #' @export
#' getSimulations.multSim <- function(x, seeds){
#'   data <- NULL
#'   times <- fromtoby(x$model@times)
#'   for(seed in seeds){
#'     a <- which(x$seeds == seed)
#'     if(length(a) == 0){
#'       warning("There are no simulations for seed ", seed)
#'     }
#'     else{
#'       d <- data.frame(seed = rep(seed, length(times)))
#'       d <- cbind(d, as.data.frame(x$outputList[[a]]))
#'       data <- rbind(data, d)
#'     }
#'   }
#'   return(data)
#' }
#' 
#' #' @rdname getData
#' #' @export
#' getSimulations.multSimCsv <- function(x, seeds){
#'   data <- NULL
#'   times <- fromtoby(x$model@times)
#'   for(seed in seeds){
#'     a <- which(x$seeds == seed)
#'     if(length(a) == 0){
#'       warning("There are no simulations for seed ", seed)
#'     }
#'     else{
#'       d <- data.frame(seed = rep(seed, length(times)), time = times)
#'       for(i in seq_along(x$lafList)){
#'         d <- cbind(d, as.numeric(x$lafList[[i]][a, ][-1]))
#'       }
#'       data <- rbind(data, d)
#'     }
#'   }
#'   colnames(data) <- c(colnames(d)[1:2], names(x$model@init))
#'   return(data)
#' }
#' 
#' #' @rdname getData
#' #' @export
#' getSimulations.pdmpModel <- function(x, seeds){ #geht das? pdmpModel ist ja s4!
#'   
#' }
#' 
#' #----------- plot methods --------------
#' 
#' #' Plot method for simulations
#' #' 
#' #' This method plots the simulation for every seed in the input. 
#' #' Continous variables are plotted as line plots, 
#' #' discrete variables in a stacked barplot above.
#' #' The plot is created with \pkg{ggplot2} and can be modified afterwards.
#' #' 
#' #' @param sdata data.frame with columns "seed", "time", and columns for the 
#' #' variables (named by the name of the variable). Method 
#' #' \code{\link{getSimulation}} returnes \code{sdata} in the required form for 
#' #' objects of class \code{\link{multSimCsv}}
#' #' @note This method requires the packages \pkg{ggplot2}, \pkg{reshape2} and 
#' #' \pkg{RColorBrewer}.
#' #' @export
#' plotSeeds_old <- function(sdata, discVarName = tail(names(sdata), 1), 
#'                       trange = c(0, tail(sdata$time, 1)), log = FALSE){
#'   
#'   if (!requireNamespace("reshape2", quietly = TRUE)) {
#'     stop("Pkg 'reshape2' needed for this function to work. 
#'          Please install it.", call. = FALSE)
#'   }
#'   if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
#'     stop("Pkg 'RColorBrewer' needed for this function to work. 
#'          Please install it.", call. = FALSE)
#'   }
#'   if (!requireNamespace("ggplot2", quietly = TRUE)) {
#'     stop("Pkg 'ggplot2' needed for this function to work. 
#'          Please install it.", call. = FALSE)
#'   }
#'   
#'   #** CMD Check preparations
#'   #* The following assignments are made to avoid the R CMD Check NOTE
#'   #* no visible binding for global variable ......
#'   #* These notes occur because surv, lower, upper, and variable are called
#'   #* within transform(), melt(), and ddply() where there is a data argument
#'   #* that R CMD check can't reconcile with the variables.
#'   value <- variable <- NULL
#'   
#'   #------ Prepare the data for plotting ------------
#'   
#'   names <- names(sdata)[!names(sdata) %in% c("seed", "time", discVarName)]
#'   
#'   for(name in discVarName){
#'     sdata[, name] <- as.factor(sdata[, name])
#'   }
#'   
#'   sdata[, "seed"] <- as.factor(sdata[, "seed"])
#'   levels(sdata[, "seed"]) <- paste0("Seed = ", levels(sdata[,"seed"]))
#'   if(length(levels(sdata[, "seed"])) > 4) 
#'     stop("To many seeds to plot. A maximum of 4 seeds can be plotted.")
#'   sdata <- reshape2::melt(sdata, id = c("time", "seed", discVarName))
#'   
#'   # define colors for discrete variables
#'   color_mapping <- function(discVar, value){
#'     index <- which(discVar == discVarName)
#'     n <- length(levels(sdata[, discVar]))
#'     levelindex <- which(value == levels(sdata[, discVar]))
#'     rainbow(length(discVarName), v = levelindex/n)[index]
#'   }
#'   discValues <- NULL
#'   for(name in discVarName){
#'     levels <- levels(sdata[, name])
#'     names(levels) <- rep(name, length(levels))
#'     discValues <- c(discValues, levels)
#'   }
#'   color_ordering <- function(int){
#'     color_mapping(names(discValues)[int], discValues[int])
#'   }
#'   for(name in discVarName){
#'     sdata[, paste0("col_", name)] <- vapply(sdata[, name], 
#'                                             function(v) color_mapping(name, v),
#'                                             character(1))
#'   }
#'   
#'   print(head(sdata))
#'   
#'   #---------- Create Plot ---------------------
#'   
#'   plot <- ggplot2::ggplot(data = sdata, ggplot2::aes(x = time))  + 
#'     ggplot2::coord_cartesian(xlim=trange) +   # zoom in
#'     ggplot2::labs(y = "", x = "time")  
#'   
#'   #** Plot discrete variables
#'   # if(log){
#'   #   plot <- plot +
#'   #     ggplot2::scale_y_log10() + 
#'   #     ggplot2::geom_rect(ggplot2::aes_string(xmin = "time", xmax = "time+1", 
#'   #                                            ymin = 0, ymax = Inf, 
#'   #                                            fill = discVarName)) +
#'   #     ggplot2::geom_hline(yintercept = 1, col = "grey")
#'   # }
#'   # else{
#'   #   plot <- plot +
#'   #     ggplot2::geom_rect(ggplot2::aes_string(xmin = "time", xmax = "time+1", 
#'   #                                            ymin = -Inf, ymax = Inf, 
#'   #                                            fill = discVarName))
#'   # }
#'   
#'   #** Plot discrete variables
#'   min <- min(sdata$value)
#'   height <- abs(max(sdata$value) - min)/10
#'   for(i in seq_along(discVarName)){
#'     plot <- plot +
#'       ggplot2::geom_rect(ggplot2::aes_string(
#'         fill = paste0("col_",discVarName[i]), 
#'         xmin = "time", xmax = "time+1", 
#'         ymin = min - i*height, ymax = min - (i - 1)*height))
#'   }
#'   plot <- plot + ggplot2::scale_fill_identity("discrete\nvariables", 
#'                                               guide = "legend", labels = printVect(discValues, collapse = NULL)
#'   )
#'   #plot <- plot + ggplot2::scale_fill_identity("discrete\nvariables", 
#'   #               guide = "legend", labels = printVect(discValues, 
#'   #               collapse = NULL), breaks = 1:length(discValues), 
#'   #               palette = color_ordering)
#'   
#'   
#'   #** Plot continous variables
#'   #cols <- RColorBrewer::brewer.pal(8, "Set1")
#'   cols <- c("#009E73", "#0072B2", "#D55E00", "#E69F00", 
#'             "#56B4E9", "#CC79A7", "#F0E442")
#'   plot <- plot + 
#'     ggplot2::geom_line(ggplot2::aes(x = time, y = value, colour = variable), 
#'                        size = 1) +
#'     ggplot2::scale_colour_manual(name = "continous\nvariables", 
#'                                  values = cols[seq_along(names)])
#'   # breaks = names, 
#'   # values=cols[1:length(names)])
#'   
#'   
#'   # facet_wrap
#'   plot <- plot + ggplot2::facet_grid(variable ~ seed)
#'   #plot <- plot + ggplot2::facet_wrap( ~ seed, ncol = 2)
#'   
#'   print(plot)
#'   invisible(plot)
#'   return(plot)
#'   }
