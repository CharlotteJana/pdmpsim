#======== todo =================================================================
#t1 example das zeigt, dass die ergebnisse von mean.multSim und 
#t1 mean.multSimCsv nicht exakt übereinstimmen

#'@rdname mean
#'@importFrom LaF colmean
#'@export
mean.multSimCsv <- function(x, ...){

  means <- NULL
  times <- fromtoby(x$model@times)
  for(j in seq_along(x$lafList)){
    meanrow <- colmean(x$lafList[[j]], columns = 2:(length(times)+1), ...)
    means <- cbind(means, meanrow)
  }
  colnames(means) <- vapply(seq_along(x$csvList), function(i)
             regmatches(x$csvList, regexec("_([^_]+).csv", x$csvList))[[i]][2],
             character(1))
  rownames(means) <- NULL
  means <- data.frame(time = times, means)
  return(means)
}

#' #'@rdname moments
#' #'@importFrom LaF colmoment
#' #'@export
#' moments.multSimCsv <- function(x, order){
#'   if(!identical(find('colmoment'),"package:LaF")) stop("Method 'colmoment' is not defined in package 'LaF'.")
#'   moments <- NULL
#'   times <- fromtoby(x$model@times)
#'   for(j in 1:length(x$lafList)){
#'     momentrow <- colmoment(x$lafList[[j]], columns = 2:(length(times)+1), order = order)
#'     moments <- cbind(moments, momentrow)
#'   }
#'   colnames(moments) <- sapply(1:length(x$csvList), function(i) regmatches(x$csvList, regexec("_([^_]+).csv", x$csvList))[[i]][2])
#'   rownames(moments) <- NULL
#'   moments <- data.frame(time = times, moments)
#'   return(moments)
#' }

