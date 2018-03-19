#' @export
print.multSimCsv <- function(x){
  cat("An S3-object of class", class(x)[1], "with elements \n\n")
  names <- names(x)
  for (name in names) {
    content <- x[name]
    if (!is.null(content)) {
      if (name == "seeds") {cat("$seeds\n"); str(x$seeds)}
      if (name == "directory") cat("$directory\n", x$directory, "\n")
      if (name == "csvList") cat("$csvList", x$csvList, sep = "\n")
      if (name == "lafList") {
        if(!is.null(x$lafList)) 
          cat("$lafList", "contains objects of class LaF", sep = "\n")
        else 
          cat("$lafList\nNULL\n")}
      if (name == "dm") {cat("$dm\n"); str(x$dm)}
      if (name == "timeList") {
        times <- vapply(x$timeList, function(i) i[1:3], numeric(3))
        if(is.null(x$timeList)) cat("$timeList\nNULL\n")
        else{
          cat("$timeList\n\n time per simulation (median):\n")
          print(apply(times, 1, median, na.rm = TRUE))
          cat("\n time altogether:\n")
          print(rowSums(times))
        }
      }
      if (name == "model")
        cat("$model\n", format(x$model, 
                               short = FALSE, 
                               slots = c("descr", "parms", "init", "times"), 
                               collapse = "\n "))
      cat("\n")
    }
  }
}
