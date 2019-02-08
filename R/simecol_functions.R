#Note: All Functions in this file were copied from package simecol

## check for from-to-by structure of vector
isfromtoby <- function(x) {
  (sum(names(x) %in% c("from","to","by"))==3)
}

## expand from-to-by-vector to sequence
fromtoby <- function(times) {
  if (isfromtoby(times)) {
    times <- seq(times["from"], times["to"], times["by"])
  } 
  times
}

setGeneric("sim", function(obj, initialize = TRUE, ...) standardGeneric("sim"))
