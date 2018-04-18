#======== todo =================================================================
# do i need calcOutliers anymore?
# move calcStats to getTimeslice.R
# example das zeigt, dass die ergebnisse von mean.multSim und mean.multSimCsv nicht exakt übereinstimmen

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


# 
# calcOutliers <- function(tdata, IQRfactor = 3){
# 
#   # compute iqr
# 
#   lowerq <- aggregate(tdata[,c(-1, -2)], list(tdata$time), quantile, 0.25)
#   upperq <- aggregate(tdata[,c(-1, -2)], list(tdata$time), quantile, 0.75)
#   iqr <- upperq - lowerq
#   #iqr <- aggregate(tdata[,c(-1,-2)], list(tdata$time), IQR)
# 
#   upper <- (iqr * IQRfactor) + upperq # upper thresholds
#   lower <- lowerq - (iqr * IQRfactor) # lower thresholds
# 
#   is.upper.outlier <- apply(tdata, 1, function(row) 
#     row[!names(row) %in% c("seed")] > upper[which(upper$Group.1 == row["time"]), ])
#   is.lower.outlier <- apply(tdata, 1, function(row) 
#     row[!names(row) %in% c("seed")] < lower[which(lower$Group.1 == row["time"]), ])
# 
#   seeds.upper <- apply(is.upper.outlier, 1, function(row) tdata$seed[row])
#   seeds.lower <- apply(is.lower.outlier, 1, function(row) tdata$seed[row])
# 
#   seeds <- sort(unique(c(unlist(seeds.upper), unlist(seeds.lower))))
# 
#   return(seeds)
# } # funktioniert nicht?

