#======== todo =================================================================
#t2 ms2msCsv: testen (auch mit append = TRUE)
#t3 Methode: msCsv2Ms schreiben
#t3 Methode: "validMsCsv" zur überprüfung, ob alles richtig ist
#t3           (einiges ist schon in multSimCsv gemacht und könnte ausgelagert 
#t3            werden, siehe auch https://www.rdocumentation.org/packages/methods/versions/3.0.3/topics/validObject)
#t1 laf zu suggests?
#t3 loadMultSimCsv: wie verlinke ich das LaF-Manual?
#t3 Wäre der parameter uniqueSeeds auch bei anderen methoden sinnvoll?
#t3 uniqueSeeds getestet?
#t3 examples für multSimCsv, loadMultSimCsv, multSim2multSimCsv
#t2 absolute paths in msCsv$csvlist?

#------------------- multSimCsv --------------------------

#' Multiple simulations of a pdmp model
#'
#' Perform simulations of a pdmp model and save 
#' the results in multiple csv files.
#'
#' @param obj object of class pdmpModel or one of its subclasses
#' @param seeds integer vector with seeds that shall be simulated
#' (different seeds lead to different simulated trajectories)
#' @param prefix string which determines the begin of all filenames that are 
#' created by the method, e.g. "NameOfModel". You can also specify a path where 
#' to save the result, e.g. "home/path/to/simulations/NameofModel".
#' @param append logical. If there already exists a file with the same filename 
#' and \code{append = TRUE}, the simulated results will be appended at the end 
#' of the file. Otherwise an error will be thrown.
#' @param uniqueSeeds logical. If some numbers in \code{seeds} appear multiple 
#' times, shall the corresponding simulations be saved only once? If 
#' append = TRUE, \code{seeds} and already simulated seeds will both be checked 
#' for repeated seed numbers.
#' @param digits integer indicating how many significant digits are to be saved 
#' in the csv files. Restricting digits can lead to smaller csv files. The 
#' default, NULL, uses \code{\link{getOption}("digits")}. 
#' @return object of class \code{multSimCsv} providing the filenames of all 
#' files that contain the simulated results with corresponding \pkg{LaF} objects 
#' stored in element \code{lafList}.
#'
#' @details
#' This method performs a simulation of the pdmpModel \code{obj} for every seed 
#' that appears in vector \code{seeds}. The result is stored in different csv 
#' files in the working directory (or any other directory if specified in 
#' \code{prefix}). Saving the results as csv files is important for large 
#' simulations where the files become bigger than the working memory of the 
#' computer. Saving as .rda object (as can be done with method 
#' \code{\link{multSim}}) then becomes useless because rda objects are loaded 
#' completely into R and therefore have to be smaller than the working memory.
#' If the simulations are stored as csv files, they can be accessed with methods
#' from package \pkg{LaF}.
#'
#' Every csv file created by \code{multSimCsv} contains the simulated values of 
#' one of the variables of the model \code{obj}.
#' If e.g. the \code{init} slot of \code{obj} is given as 
#' \code{c(f = 1, g = 2, h = 3)} and \code{prefix = "exampleModel"}, 
#' method \code{multSimCsv} will create three csv files named 
#' \code{exampleModel_Simulations_f.csv}, \code{exampleModel_Simulations_g.csv},
#' and \code{exampleModel_Simulations_h.csv} where the first contains all 
#' simulated values of variable \code{f}, the second of \code{g} and the third 
#' of \code{h}.
#'
#' The csv files are constructed as follows:
#' \itemize{
#' \item The first row contains the names of the columns. The first column is 
#' named "Seed", all other column names come from the time values, e. g. "time0" 
#' "time0.1" "time0.2" and so on.
#' \item All other rows contain the seed (first column) and the corresponding 
#' simulation for one of the variables.
#' }
#'
#' Slot \code{obj@out} is not affected by \code{multSim}.
#' In case of a break or an error, already simulated seeds will still be saved 
#' in the csv files.
#'
#' Method \code{multSimCsv} also saves and returnes an object of s3 class 
#' \code{multSimCsv} which contains all important informations about the 
#' simulation and can be loaded into the working memory. In our example, 
#' this object would be saved as "exampleModel_MultSimCsv.rda". 
#' The class has 6 elements:
#' \itemize{
#' \item \code{model} containing the pdmpModel \code{obj},
#' \item \code{seeds} containing the integer vector \code{seeds},
#' \item \code{csvList}: a character vector containing the filenames of 
#' stored simulated results,
#' \item \code{datamodel} containing all the information package \pkg{LaF} needs 
#' for opening the csv files,
#' \item \code{lafList}: a list for the corresponding LaF objects,
#' \item \code{timeList}: a list containing the time needed for every simulation
#' }
#' Element \code{lafList} has to be created during run-time, therefore it is 
#' saved as null. The returned \code{multSimCsv} object however has a valid 
#' \code{lafList}. If you want to load a \code{multSimCsv} object from a rda
#' file, use \code{\link{loadMultSimCsv}} to create \code{lafList} automatically.
#' @seealso \code{\link{loadMultSimCsv}} to load a stored \code{multSimCsv} 
#' object, \code{multSim} to perform multiple simulations that need not
#' too much memory, \code{\link{multSim2multSimCsv}} to convert an object
#' created with method \code{multSim} into a \code{multSimCsv} object.
#' 
#' @section Working with the simulations:
#' 
#' If you stored your simulations in csv files with \code{multSimCsv}, you
#' can access the results by loading the corresponding \code{rda} object
#' with \code{\link{loadMultSimCsv}} and store them in a variable, i.e. 
#' \code{msCsv}. Then you can read out i.e. the simulated values of 
#' the second variable (referring to the order of your variables in 
#' slot \code{init}) with the third seed via \code{msCsv$lafList[[2]][3, ]}.\cr
#' 
#' There are also a lot of plot methods available. To use them, you first
#' have to choose some seed numbers and time values and use method
#' \code{\link{getMultSimData}} to store them in a \code{data.frame}. 
#' This method may need some time for calculation and the result needs to
#' fit into the working memory. Afterwards, you can apply methods
#' \code{\link{plot}}, \code{\link{plotTimes}}, \code{\link{plotStats}},
#' \code{\link{summarize_at}}, \code{\link{hist}}, \code{\link{density}} 
#' and \code{\link{plotSeeds}} on the data.frame created with 
#' \code{getMultSimData}. You can also use \code{\link{plotTimes}} to find 
#' the seed numbers of interesting outliers by setting an appropriate threshold.
#' 
#' \code{}
#' @aliases multsimcsv
#' @importFrom utils write.table
#' @export
multSimCsv <- function(obj, seeds, prefix = format(obj, end = "__"), 
                       append = FALSE, uniqueSeeds = TRUE, digits = NULL){
  
  if(class(obj) == "pdmpBorder")
    stop("Method 'multSimCsv' is not implemented for objects of class 'pdmpBorder'.")

  if(substr(prefix, nchar(prefix)-3, nchar(prefix)) == ".csv"){
    prefix <- substr(prefix, 1, nchar(prefix)-4)
  }

  times <- do.call(seq, as.list(obj@times))
  colnames <- c("Seed", paste0("time", times))
  names <- names(obj@init)
  csvNames <- paste0(prefix, "_Simulations_", names, ".csv")
  multSimName <- paste0(prefix, "_MultSimCsv.rda")
  timeList <- list()
  # load existing simulations and check them
  checkFiles <- file.exists(csvNames)
  if(sum(checkFiles) != 0){ # if some files already exist
    if(!append)
      stop("Files ", paste(csvNames[checkFiles], collapse = ", "), 
           " already exist.")
    if(sum(checkFiles) != length(csvNames))
      stop("Only ", paste(csvNames[checkFiles], collapse = ", "), 
          " exist but not ", paste(csvNames[!checkFiles], collapse = ", "), ".")
    if(!file.exists(multSimName))
      stop("Files ", paste(csvNames[checkFiles], collapse = ", "), 
           " exist but not ", multSimName, ". ")
    msCsvOld <- loadMultSimCsv(multSimName)
    if(!all.equal(msCsvOld$model,obj)) {
      rm(msCsvOld)
      stop("The pdmp model stored in ", multSimName, "differs from 'obj'.")
    }
    oldSeeds <- NULL
    for(i in seq_along(msCsvOld$lafList)){
      oldSeeds <- cbind(oldSeeds, unlist(msCsvOld$lafList[[i]][,1]))
    }
    oldSeeds <- unique(oldSeeds, MARGIN = 2)
    if(ncol(oldSeeds) != 1) stop("Seed numbers in .csv files do not agree")
    if(!identical(as.vector(oldSeeds), unique(as.vector(oldSeeds)))){
      if(uniqueSeeds) stop("Seeds in .csv files contain repeated values.")
      else warning("Seeds in .csv files contain repeated values.")
    }
  }

  # write colnames
  if(!append){
    for(j in seq_along(names)){
      utils::write.table(t(colnames), csvNames[j], sep = ",", 
                         append = FALSE, row.names = FALSE, col.names = FALSE)
    }
  }
  if(uniqueSeeds){
    seeds <- unique(seeds)
    if(exists("oldSeeds")){
      seeds <- seeds[!seeds %in% oldSeeds]
      if(length(seeds) == 0) stop("All seeds have already been simulated.")
    }
  }
  calcStart <- TRUE
  for(i in seq_along(seeds)){

    # message for calculation
    if(calcStart) {
      message("Calculating seeds ", appendLF = FALSE)
      calcStart <- FALSE
    }
    if(i == 1 || i %% ceiling(length(seeds)/20) == 0)
      message(" ", seeds[i], sep = " ", appendLF = FALSE)
    else if(i %% ceiling(length(seeds)/100) == 0)
      message(".", appendLF = FALSE)

    # simulation
    timeList[[i]] <- system.time(
      out <- sim(obj, seed = seeds[i], outSlot = FALSE)
    )

    # export to csv
    for(j in seq_along(names)){
      utils::write.table(format(t(c(seeds[i], out[,j+1])), digits = digits), 
                         csvNames[j], sep = ",", append = TRUE, 
                         row.names = FALSE, col.names = FALSE)
    }
  }
  if(i == length(seeds)) message("")


  # create object of class "multSimCsv"
  coltypes <- rep("double", length(times)+1)
  datamodel <- list(type = "csv", skip = 1, dec = ".", sep = ",",
   columns = data.frame(name = colnames, type = coltypes, 
                        stringsAsFactors = FALSE))
  if(exists("msCsvOld")){
    timeList <- c(msCsvOld$timeList, timeList)
    seeds <- c(oldSeeds, seeds)
  }
  msCsv  <- structure(list(directory = getwd(),
                           seeds = seeds,
                           timeList = timeList,
                           csvList = csvNames,
                           datamodel = datamodel,
                           lafList = NULL,
                           model = obj),
                      class = "multSimCsv")
  saveRDS(msCsv, file = multSimName)
  
  #load laf
  msCsv$lafList <- list(0)
  for(i in seq_along(msCsv$csvList)){
    msCsv$lafList[[i]] <- laf_open(msCsv$datamodel, file = csvNames[[i]])
  }
  
  return(msCsv)
}

#------------ loadMultSimCsv -------------------

#' load csv files with simulation results
#'
#' Load an object of class \code{multSimCsv} created with method
#' \code{\link{multSimCsv}} from a directory and create LaF links to
#' the stored csv files (\pkg{LaF} is a package that provides a fast access
#' to data stored in csv files). The method returnes an object of class
#' \code{multSimCsv} with the LaF links stored in element \code{lafList}.
#' 
#' @param x either an object  of class \code{\link{multSimCsv}} or a character 
#' string indicating the filename of a stored multSimCsv object, e. g. 
#' "exampleModel_MultSimCsv.rda"
#' @param dir directory where the csv files are stored. 
#' Defaults to the working directory.
#' @return object of class \code{\link{multSimCsv}}
#' @note Opening the saved rda file directly with \code{\link{readRDS}} will 
#' lead to a \code{multSimCsv} object with \code{lafList} set to NULL.
#' The links have to be created during run-time which is the reason why
#' method \code{loadMultSimCsv} exists.
#' @seealso \code{\link{multSimCsv}} to create the csv files, 
#' \code{vignette("LaF-manual")} for an introduction of package \pkg{LaF}
#' @aliases loadmultsimcsv
#' @importFrom LaF laf_open
#' @export
loadMultSimCsv <- function(x, dir){
  UseMethod("loadMultSimCsv", x)
}

#' @describeIn loadMultSimCsv create laf-links for multSimCsv objects
loadMultSimCsv.multSimCsv <- function(x, dir = "."){
  x$lafList <- list(0)
  for(i in seq_along(x$csvList)){
    x$lafList[[i]] <- laf_open(x$datamodel, 
      file = normalizePath(file.path(dir, x$csvList[[i]])))
  }
  return(x)
}

#' @describeIn loadMultSimCsv load multSimCsv from rda file and create LaF links
loadMultSimCsv.character <- function(x, dir = "."){
  msCsv <- readRDS(file = normalizePath(file.path(dir, x)))
  msCsv$lafList <- list(0)
  for(i in seq_along(msCsv$csvList)){
    msCsv$lafList[[i]] <- laf_open(msCsv$datamodel, 
      file = normalizePath(file.path(dir, msCsv$csvList[[i]])))
  }
  return(msCsv)
}

#------------ multSim2multSimCsv -------------------

#' Convert 'multSim' to 'multSimCsv'
#' 
#' Convert an object of class \code{\link{multSim}} to an object of class 
#' \code{\link{multSimCsv}}. This can be useful if it turns out that the 
#' \code{\link{multSim}} object needs to much working memory.
#' @param ms object of class \code{\link{multSim}}
#' @inheritParams multSimCsv
#' @return object of class \code{\link{multSimCsv}} providing the filenames of 
#' all csv files that contain the simulated results with corresponding \pkg{LaF} 
#' objects stored in element \code{lafList}. 
#' @aliases multsim2multsimcsv
#' @importFrom utils write.table
#' @export
multSim2multSimCsv <- function(ms, prefix = format(ms$model), 
                               append = FALSE, digits = NULL){
  
  if(substr(prefix, nchar(prefix)-3, nchar(prefix)) == ".csv"){
    prefix <- substr(prefix, 1, nchar(prefix)-4)
  }
  
  ms <- removeSeeds(ms)
  times <- fromtoby(ms$model@times)
  names <- names(ms$model@init)
  colnames <- c("Seed", paste0("time", times))
  csvNames <- paste0(prefix, "___Simulations_", names, ".csv")
  if(!append && sum(file.exists(csvNames)) != 0) 
   stop("Files ", paste(csvNames[file.exists(csvNames)], collapse = ", "), 
        " already exist.")
  
  # write colnames
  if(!append){
    for(j in seq_along(names)){
      utils::write.table(t(c("Seed", times)), csvNames[j], 
                         sep = ",", row.names = FALSE, col.names = FALSE)
    }
  }
  
  # write simulations as rows
  for(i in seq_along(ms$outputList)){
    for(j in seq_along(names)){
      utils::write.table(format(t(c(ms$seeds[i], ms$outputList[[i]][,j+1])), 
                                digits = digits), csvNames[j], sep = ",", 
                         append = TRUE, row.names = FALSE, col.names = FALSE)
    }
  }
  
  # create object of class "multSimCsv"
  
  coltypes <- rep("double", length(times)+1)
  datamodel <- list(type = "csv", skip = 1, dec = ".", sep = ",",
                    columns = data.frame(name = colnames, 
                                         type = coltypes, 
                                         stringsAsFactors = FALSE))
  
  msCsv  <- structure(list(directory = getwd(),
                           seeds = ms$seeds,
                           timeList = ms$timeList,
                           csvList = csvNames,
                           datamodel = datamodel,
                           lafList = NULL,
                           model = ms$model),
                      class = "multSimCsv")
  saveRDS(msCsv, file = paste0(prefix, "_MultSimCsv.rda"))
  msCsv <- loadMultSimCsv(msCsv)
  return(msCsv)
}
