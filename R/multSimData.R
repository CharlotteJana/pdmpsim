#======== todo =================================================================
#t2 tests schreiben
#t2 methode für multSimCsv sehr langsam, eventuell umschreiben?

#' @include pdmp_class.R pdmp_sim.R multSim.R msCsv.R
NULL

#----------- getMultSimData --------------

#' Create data.frame with simulations
#'
#' This method transforms objects of class \code{multSim} or \code{multSimCsv}
#' into a specific data.frame of class \code{multSimData}. Most plot method
#' of package \pkg{pdmpsim} are based on this class. The data.frame has five
#' columns:
#' \itemize{
#' \item \code{time} for the time value,
#' \item \code{seed} for the seed on which the simulation was created,
#' \item \code{type} indicating if the variable is discrete or continous,
#' \item \code{variable} for the name of the simulated variable,
#' \item \code{value} for the simulated value.} 
#' Methods that use objects of class \code{multSimData} as input are
#' \code{\link[=plot.multSimData]{plot}}, \code{\link{plotSeeds}}, \code{\link{plotTimes}},
#' \code{\link{plotStats}}, \code{\link{hist}} and \code{\link{density}}.
#' 
#' @param x an object of class \code{\link{multSim}}, \code{\link{multSimCsv}},
#' \code{multSimData} or \code{\link{pdmpModel}}
#' @param seeds vector with specific seeds for which the simulation
#' results (for all times) shall appear in the data.frame
#' @param times vector with specific time values for which the simulation
#' results (for all seeds) shall appear in the data.frame
#' @return a data.frame of class \code{multSimData} with simulation results.
#' @examples 
#' data("toggleSwitch")
#' ms <- multSim(toggleSwitch, seeds = 1:5)
#' d <- getMultSimData(ms, seeds = 1:3, times = c(5, 10))
#' print(d)
#' @name multSimData
#' @aliases getmultsimdata multSimData multsimdata
#' @export
getMultSimData <- function (x, times, seeds)  {
  UseMethod("getMultSimData", x)
}

#' @rdname multSimData
#' @importFrom dplyr select everything
#' @export
getMultSimData.multSim <- function(x, times, seeds){
  x <- removeSeeds(x)
  data <- NULL
  timeIndex <- NULL
  seedIndex <- NULL
  all.times <- fromtoby(x$model@times)
  
  if(class(x$model) == "mjpModel"){
    discVarNames <- NULL
  }
  else{
    discVarNames <- names(discStates(x$model))
  }
  
  # to avoid the R CMD Check NOTE 'no visible binding for global variable ...'
  time <- seed <- type <- variable <- value <- NULL
  
  if(missing(times) || is.null(times)){
    times <- all.times
    timeIndex <- seq_along(all.times)
  }
  if(missing(seeds) || is.null(seeds)){
    seeds <- x$seeds
    seedIndex <- seq_along(seeds)
  }
  
  # select times
  if(!identical(times, all.times)){
    for(i in times){
      a <- which(all.times == i)
      if(length(a) == 0){
        warning("There are no simulations for time ", i)
      }
      else 
        timeIndex <- c(timeIndex, a)
    }
  }
  
  # select seeds
  if(!identical(seeds, x$seeds)){
    for(s in seeds){
      a <- which(x$seeds == s)
      if(length(a) == 0){
        warning("There are no simulations for seed ", s)
      }
      else
        seedIndex <- c(seedIndex, a)
    }
  }
  else{
    seedIndex <- seq_along(seeds)
  }
  
  for(j in seq_along(seedIndex)){
    lastTime <- nrow(x$outputList[[seedIndex[j]]])
    seedData <- x$outputList[[seedIndex[j]]][timeIndex[timeIndex %in% 1:lastTime], ]
    if(class(seedData) == "numeric")
      seedData <- t(seedData)
    data <- rbind(data, cbind(seed = rep(x$seeds[seedIndex[j]]), seedData))
  }
  
  # every value gets its own row
  data <- as.data.frame(data)
  data <- reshape2::melt(data, id = c("time", "seed"))
  
  # column "type" = continous / discrete
  data$type <- as.factor(
    vapply(data$variable, 
           function(v) ifelse(v %in% discVarNames, "disc", "cont"),
           character(1)))
  
  # column order = time, seed, type, variable, value
  data <- select(as.data.frame(data), 
                 time, seed, type, variable, value)
  attr(data, "class") <- c("multSimData", class(data))
  return(data)
}

#' @rdname multSimData
#' @export
getMultSimData.multSimCsv <- function(x, times, seeds){
  data <- data.frame(time = numeric(),
                     seed = numeric(),
                     type = character(),
                     variable = character(),
                     value = numeric(),
                     stringsAsFactors = FALSE)
  timeIndex <- NULL
  seedIndex <- NULL
  all.times <- fromtoby(x$model@times)
  discVarNames <- names(discStates(x$model))
  
  if(missing(times) || is.null(times)){
    times <- all.times
    timeIndex <- seq_along(all.times)
  }
  if(missing(seeds) || is.null(seeds)){
    seeds <- x$seeds
    seedIndex <- seq_along(seeds)
  }
  
  # select times
  if(!identical(times, all.times)){
    for(i in times){
      a <- which(all.times == i)
      if(length(a) == 0){
        warning("There are no simulations for time ", i)
      }
      else 
        timeIndex <- c(timeIndex, a)
    }
  }
  
  # select seeds
  if(!identical(seeds, x$seeds)){
    for(s in seeds){
      a <- which(x$seeds == s)
      if(length(a) == 0){
        warning("There are no simulations for seed ", s)
      }
      else
        seedIndex <- c(seedIndex, a)
    }
  }
  else
    seedIndex <- seq_along(seeds)

  ### diese for-Schleife macht das Ganze so langsam!!!
  for(n in seq_along(init(x$model))){
    varName <- names(init(x$model))[n]
    for(i in seq_along(timeIndex)){
      for(j in seq_along(seedIndex)){
        value <- as.numeric(x$lafList[[n]][seedIndex[j], timeIndex[i]+1])
        new_row <- data.frame(time = all.times[timeIndex[i]],
                              seed = x$seeds[seedIndex[j]],
                              type = ifelse(is.element(varName, discVarNames),
                                            "disc", "cont"),
                              variable = varName,
                              value = value)
        data <- rbind(data, new_row)
      }
    }
  }
  attr(data, "class") <- c("multSimData", class(data))

  return(data)
}

#' @rdname multSimData
#' @export
getMultSimData.multSimData <- function(x, times, seeds){
  
  # to avoid the R CMD Check NOTE 'no visible binding for global variable ...'
  time <- seed <- NULL
  
  if(missing(times) || is.null(times))
    times <- unique(x$time)
  if(missing(seeds) || is.null(seeds))
    seeds <- unique(x$seed)
  x <- subset(x, time %in% times & seed %in% seeds)
  return(x)
}

#' @rdname multSimData
#' @export
setMethod("getMultSimData", signature(x="pdmpModel"),
          function(x, times, seeds) {

            if(missing(seeds) || is.null(seeds)) 
              seeds <- NA
            if(length(seeds) != 1)
              stop("Error in getMultSimData: parameter seeds should contain maximal one
                   number, because pdmpModel@out contains maximal one simulation.")
            
            if(is.null(out(x)))
              stop("Please simulate the model before applying 'getMultSimData'.")
            
            timeIndex <- NULL
            all.times <- fromtoby(x@times)
            discVarNames <- names(discStates(x))
            
            if(missing(times) || is.null(times)){
              times <- all.times
              timeIndex <- seq_along(all.times)
            }
            
            # to avoid the R CMD Check NOTE 'no visible binding for global variable ...'
            time <- seed <- type <- variable <- value <- NULL
            
            
            
            # select times
            if(!identical(times, all.times)){
              for(i in times){
                a <- which(all.times == i)
                if(length(a) == 0){
                  warning("There are no simulations for time ", i)
                }
                else 
                  timeIndex <- c(timeIndex, a)
              }
            }
            
            data <- x@out[timeIndex, , drop = FALSE]
            data <- cbind(seed = rep(seeds), data)
            
            # every value gets its own row
            data <- as.data.frame(data)
            data <- reshape2::melt(data, id = c("time", "seed"))
            
            # column "type" = continous / discrete
            data$type <- as.factor(
              vapply(data$variable, 
                     function(v) ifelse(v %in% discVarNames, "disc", "cont"),
                     character(1)))
            
            # column order = time, seed, type, variable, value
            data <- select(as.data.frame(data), 
                           time, seed, type, variable, value)
            attr(data, "class") <- c("multSimData", class(data))
            return(data)
})

is.multSimData <- function(x){
  b1 <- identical(colnames(x), c("time", "seed", "type", "variable", "value"))
  b2 <- is.element("multSimData", class(x))
  return(b1 & b2)
}
