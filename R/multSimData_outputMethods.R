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