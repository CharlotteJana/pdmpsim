#======== todo =================================================================
#t2 removeSeeds: allgemein für bestimmte Nummern? Auch für msCsv machen?
#t2 removeSeeds: message?
#t1 Problem bei mean Funktionen: Es gibt NA simulations (Fehlermeldung angeben!!!)
#t1 na.rm als variable einfügen -> dann vllt kein aufruf von removeSeeds mehr nötig?
#t3 mean mit ... testen
#t1 toggleSwitch -> mean(dA) sollte gleich sein für msCsv und ms!
#t3 examples für mean, removeSeeds, moments
#t1 example das zeigt, dass die ergebnisse von mean.multSim und 
#t1 mean.multSimCsv nicht exakt übereinstimmen


#' @include pdmp_class.R pdmp_sim.R multSim.R
NULL

#' Remove not simulated seeds
#'
#' If a simulation via \code{\link{multSim}} has been interrupted, slot 
#' \code{outputList} of the returned \code{multSim} object contains NAs. 
#' Method \code{removeSeeds} removes these NA entries and changes all other 
#' slots accordingly. This method is used internally for other calculation 
#' methods as e.g. \code{\link{mean}}.
#' @param ms object of class \code{\link{multSim}}
#' @return object of class \code{multSim} that does not contain NAs in slot 
#' \code{outputList}
#' @aliases removeseeds
#' @export
removeSeeds <- function(ms){
  bools <- !is.na(ms$outputList)
  if(sum(!bools) != 0) 
    message("For some seeds no simulations existed. They have been removed.")
  return(structure(list(seeds = ms$seeds[bools],
                        outputList = ms$outputList[bools],
                        timeList = ms$timeList[bools],
                        model = ms$model),
                   class = "multSim"))
}

#' Methods for function \code{mean} in Package \pkg{pdmpsim}
#'
#' This method calculates the mean for every time value over all simulations.
#' @return data.frame with calculated mean values
#' @param x object of class \code{\link{multSim}} or \code{\link{multSimCsv}}
#' @param ... additional arguments passed to \code{\link[base]{mean}}
#' @rdname mean
#' @name mean
#' @export
mean.multSim <- function(x, ...){

  ms <- removeSeeds(x)
  times <- fromtoby(x$model@times)
  means <- NULL
  for(j in seq_len(ncol(ms$outputList[[1]]))){
    h <- vapply(ms$outputList, function(out) out[,j], numeric(length(times)))
    means <- cbind(means, rowMeans(h))
  }
  colnames(means) <- colnames(ms$outputList[[1]], ...)
  return(data.frame(means))
}

#' Calculate raw moments
#'
#' This method calculates the raw moment of a given order for every time 
#' over all simulations.
#' @param x object of class \code{\link{multSim}} or \code{\link{multSimCsv}}
#' @param order number that specifies the order of the moments
#' @return data.frame with calculated moments
#' @rdname moments
#' @export
moments <- function(x, order){
  UseMethod("moments", x)
}

#' @rdname moments
#' @export
moments.multSim <- function(x, order){

  x <- removeSeeds(x)
  times <- fromtoby(x$model@times)
  moments <- NULL
  for(j in 2:ncol(x$outputList[[1]])){
    h <- vapply(x$outputList, function(out) out[, j], numeric(length(times)))
    momentvector <- apply(h, 1, function(row) sum(row^order)/length(row))
    moments <- cbind(moments, momentvector)
  }
  moments <- cbind(times, moments)
  colnames(moments) <- colnames(x$outputList[[1]])
  return(data.frame(moments))
}

#' @rdname summarise_at
#' @export
summarise_at.multSim <- function(.tbl, .vars, .funs, ...){
  x <- getMultSimData(.tbl)
  summarise_at(x, .vars, .funs, ...)
}