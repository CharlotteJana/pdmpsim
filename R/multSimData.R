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
  all.times <- do.call(seq, as.list(x$model@times))
  
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

#' @rdname getData
#' @export
getTimeslice.multSimCsv <- function(x, times){
  data <- NULL
  seeds <- x$seeds
  all.times <- fromtoby(x$model@times)
  index <- NULL
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
