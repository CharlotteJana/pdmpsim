#======== todo =================================================================
# discVarNames rausnehmen und durch slot ersetzen

#' @include pdmp_class.R pdmp_sim.R multSim.R msCsv.R
NULL

#----------- getMultSimData --------------

#' @name multSimData
#' @aliases getmultsimdata multSimData multsimdata
#' @export
getMultSimData <- function (x, times, seeds, discVarNames)  {
  UseMethod("getMultSimData", x)
}

#' @rdname multSimData
#' @importFrom dplyr select everything
#' @export
getMultSimData.multSim <- function(x, times, seeds, discVarNames){
  data <- NULL
  timeIndex <- NULL
  seedIndex <- NULL
  all.times <- fromtoby(x$model@times)
  
  if(missing(times) || is.null(times)){
    times <- all.times
    timeIndex <- seq_along(all.times)
  }
  if(missing(seeds) || is.null(seeds)){
    seeds <- x$seeds
    seedIndex <- seq_along(seeds)
  }
  if(missing(discVarNames)){
    discVarNames <- NULL
    for(i in seq_along(x$model@init)){
      if(length(unique(x$outputList[[1]][, i+1])) < 6) 
        discVarNames <- cbind(discVarNames, names(init(x$model))[i])
    }
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
  
  for(i in seq_along(timeIndex)){
    for(j in seq_along(seedIndex)){
      data <- rbind(data, c(seed = seeds[j], 
                            x$outputList[[seedIndex[j]]][timeIndex[i], ]))
    }
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
getMultSimData.multSimCsv <- function(x, times, seeds, discVarNames){
  data <- data.frame(time = numeric(),
                     seed = numeric(),
                     type = character(),
                     variable = character(),
                     value = numeric(),
                     stringsAsFactors = FALSE)
  timeIndex <- NULL
  seedIndex <- NULL
  all.times <- fromtoby(x$model@times)
  
  if(missing(times) || is.null(times)){
    times <- all.times
    timeIndex <- seq_along(all.times)
  }
  if(missing(seeds) || is.null(seeds)){
    seeds <- x$seeds
    seedIndex <- seq_along(seeds)
  }
  if(missing(discVarNames)){
    discVarNames <- NULL
    for(i in seq_along(x$model@init)){
      if(length(unique(x$outputList[[1]][, i+1])) < 6) 
        discVarNames <- cbind(discVarNames, names(init(x$model))[i])
    }
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
  
  for(n in seq_along(init(x$model))){
    varName <- names(init(x$model))[n]
    for(i in seq_along(timeIndex)){
      for(j in seq_along(seedIndex)){
        value <- as.numeric(x$lafList[[n]][seedIndex[j], timeIndex[i]+1])
        new_row <- data.frame(time = all.times[timeIndex[i]],
                              seed = x$seeds[seedIndex[j]],
                              type = ifelse(varName %in% discVarNames, 
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
