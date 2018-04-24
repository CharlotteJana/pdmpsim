#======== todo =================================================================
#t2 multSim: filename missing -> nichts abspeichern?? + warning!
#t3 wirklich Fehlermeldung geben, wenn nur die Slot descr geändert wird?

#' @include pdmp_class.R pdmp_sim.R
NULL

#' Multiple simulations of a pdmp model
#'
#' Perform simulations of a pdmp model with different seeds and optionally save 
#' the results in a rda file.
#' @param obj object of class pdmpModel or one of its subclasses
#' @param seeds integer vector with seeds that shall be simulated
#' (different seeds lead to different simulated trajectories)
#' @param filename string which indicates the path where to save the result, 
#' i. e. "NameOfFile.rda". In case of a break or an error, already simulated 
#' seeds will still be saved there. If \code{filename} = NULL, the result will
#' not be saved in a file.
#' @param ms object of class \code{multSim} (with existing simulations) that 
#' shall be expanded
#' @param allowDeletion logical. If true, seeds with existing simulations 
#' stored in \code{ms} will be deleted in case they don't appear in 
#' the new vector of seeds.
#' @return object of class \code{multSim} containing simulations for all 
#' given seeds
#'
#' @details The returned s3 class \code{multSim} contains 4 different elements:
#' \itemize{
#' \item \code{model} containing the pdmpModel \code{obj},
#' \item \code{seeds} containing the integer vector \code{seeds},
#' \item a list \code{outputList} containing the simulation results,
#' i.e. \code{outputList[[i]]} = \code{sim(model, seed = seeds[i])},
#' \item a list \code{timeList} containing the time needed for every simulation,
#' i.e. \code{timeList[[i]]} = \code{system.time(sim(model, seed = seeds[i]))}
#' }
#' If an object \code{ms} of class \code{multSim} is given as parameter, method 
#' \code{multSim} will only simulate the seeds that are not simulated yet. 
#' You can control if seeds that are already simulated and stored in \code{ms} 
#' but do not appear in the parameter \code{seeds} should be deleted or not. 
#' This is done via the parameter \code{allowDeletion}.
#'
#' If the \code{time} slot of \code{obj} has a larger end value than the 
#' pdmpModel stored in \code{ms}, i.e. \code{times(obj)["to"] >} 
#' \code{times(ms$model)["to"]}, all existing simulations will be expanded to 
#' the larger end value.
#'
#' Slot \code{obj@out} is not affected by \code{multSim}.
#' @export
multSim <- function(obj, seeds, filename = NULL, 
                    ms = NULL, allowDeletion = FALSE){

  # check if filename ends with ".rda"
  if(!is.null(filename) && 
     substr(filename, nchar(filename)-3, nchar(filename)) != ".rda"){
    stop("Variable \"filename\" should end with \".rda\". 
         For example \"NameOfFile.rda\".")
  }

  # if ms is not given: initialization
  if(is.null(ms)){
    outputList <- as.list(rep(NA, length(seeds)))
    timeList <- as.list(rep(NA, length(seeds)))
    ms <- structure(list(seeds = seeds,
                         outputList = outputList,
                         timeList = timeList,
                         model = obj),
                    class = "multSim")
  }

  # compare and expand times
  if(all(obj@times[c("from", "by")] == ms$model@times[c("from", "by")])){
    if(obj@times["to"] > ms$model@times["to"]){
      message("Parameter \'times(ms$model)[\"to\"]\' has changed from ", 
              ms$model@times[["to"]], " to ", obj@times[["to"]], ".")
      ms <- expandTimes(ms, obj@times[["to"]])
    }
  }

  # compare obj with ms
  for(name in slotNames(obj)){
    if(!isTRUE(all.equal(slot(obj, name), slot(ms$model, name), tolerance=0))){
      stop("Parameter \'obj@", name,
           "\' does not agree with \'ms$model@", name, "\'.")
    }
  }

  # if all is identical: break
  if(identical(seeds, ms$seeds) && all(!is.na(ms$outputList))){
    message("Already calculated.")
    if(is.null(filename)) return(ms)
    else return(NULL)
  }

  # compare seeds and simulate
  if(!identical(seeds, ms$seeds))
    message("Parameter \'ms$seeds\' has changed.")
  ms <- calcSeeds(ms, seeds, filename, allowDeletion) #simulation


  # return
  if(is.null(filename)) return(ms)
}

#' Calculate Seeds
#'
#' Simulates all seeds in a given vector that are not simulated yet.
#' @inheritParams multSim
#' @return object of class "multSim" containing all simulations
#'
#' @keywords internal
#' @seealso multSim
calcSeeds <- function(ms, seeds, filename = NULL, allowDeletion = FALSE){

  if(!is.null(filename) && 
     substr(filename, nchar(filename) - 3, nchar(filename)) != ".rda"){
    stop("Variable \"filename\" should end with \".rda\". 
         For example \"NameOfFile.rda\".")
  }

  names(seeds) <- NULL
  names(ms$seeds) <- NULL
  if(!identical(seeds, ms$seeds) && !is.null(ms$seeds)){


    if(sum(is.na(match(ms$seeds, seeds))) != 0){
      if(allowDeletion)
        message("Seeds ", 
                paste(ms$seeds[is.na(match(ms$seeds, seeds))], collapse = ", "),
                " were previously calculated and will be deleted. \n",
                "If you want to preserve them, try again with option 
                 \'allowDeletion = FALSE\'.")
      else
        message("Seeds ", 
                paste(ms$seeds[is.na(match(ms$seeds, seeds))], collapse = ", "),
                " were previously calculated and will be preserved. \n",
                "If you want to delete them, try again with option 
                \'allowDeletion = TRUE\'.")
    }

    if(!allowDeletion){
      seeds <- unique(c(seeds, ms$seeds))
    }

    sameSeeds <- match(seeds, ms$seeds) # position of entries of variable 
                                        # "seeds" in variable "ms$seeds" 
                                        # (no match → position = NA)
    outCopy <- ms$outputList[sameSeeds] # save outputs for common seeds
    timeCopy <- ms$timeList[sameSeeds] # save times needed for calculation

    ms$outputList <- as.list(rep(NA, length(seeds))) #delete existing outputList
    ms$timeList <- as.list(rep(NA, length(seeds))) #delete existing timeList

    lapply(seq_along(seeds), function(i) { # copy the values of common seeds
      if(!is.na(sameSeeds[i])) ms$outputList[i] <<- outCopy[i]
      if(!is.na(sameSeeds[i])) ms$timeList[i] <<- timeCopy[i]
    })
    ms$seeds <- seeds
  }

  # simulate additional seeds
  tryCatch({

    calcStart <- TRUE

    for(i in seq_along(seeds)){
      if(all(is.na(ms$outputList[[i]]))){ # if output does not exist

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
        ms$timeList[[i]] <- system.time( # stores time needed for simulation
          ms$outputList[[i]] <- sim(ms$model, seed = seeds[i], outSlot = FALSE)
        )
      }
      if(i == length(seeds)) message("")
    }},
    finally = {
      if(!is.null(filename)){
        saveRDS(ms, file = filename)
        message("Result is stored in ", paste(getwd(),"/", filename, sep = ""))
      }
      return(ms)
    }
  )
}

#' Expand simulation time
#'
#' Expands all existing simulations to a longer time period
#' @param ms object of class "multSim" (with existing simulations) that shall 
#' be expanded (simulations are in ms$outputList; every NA in ms$outputList 
#' will be skipped and not simulated)
#' @param to new stopping time, should be larger than times(ms$model)["to"]
#' @return object of class "multSim" containing the simulations
#'
#' @keywords internal
#' @seealso multSim
expandTimes <- function(ms, to){
  if(to <= ms$model@times["to"]) 
    stop("Value \"to\" has to be larger than ", ms$model@times["to"],".")

  # initialization of "newMS", an object of class "multSim" that will contain 
  # (only) the simulations of the additional times
  newModel <- ms$model
  newModel@times[["to"]] <- to
  newMS  <- structure(list(seeds = ms$seeds,
                           outputList = as.list(rep(NA, length(ms$outputList))),
                           timeList = as.list(rep(NA, length(ms$timeList))),
                           model = newModel),
                      class = "multSim")

  # for every seed, there is an other initial value 
  # ( = the last value of the existing output for this seed)
  initList <- lapply(ms$outputList, function(output) 
                if(all(is.na(output))) NA 
                else tail(output, n=1)[, -1])

  # simulate additional times
  tryCatch({

    message("Expanding Time for seeds ", appendLF = FALSE)

    for(i in seq_along(newMS$seeds)){
      if(!all(is.na(ms$outputList[[i]]))){ # if output does not exist

        # message for calculation
        if(i == 1 || i %% ceiling(length(newMS$seeds)/20) == 0) 
          message(" ", newMS$seeds[i], sep = " ", appendLF = FALSE) 
        else if(i %% ceiling(length(newMS$seeds)/100) == 0) 
          message(".", appendLF = FALSE)

        newMS$model@init <- initList[[i]] # change initial value
        newMS$timeList[[i]] <- system.time( # stores time needed for simulation
          newMS$outputList[[i]] <- sim(newMS$model, 
                                       seed = newMS$seeds[i], 
                                       outSlot = FALSE)
        )
      }
      if(i == length(newMS$seeds)) message("")
    }},
    finally = {
      # if an error occured: give warning and return ms
      if(i != length(ms$seeds)){
        warning("Calculation of method \"expandTimes\" could not be finished. 
                 The original value for \"ms\" is returned.")
        return(ms)
      }
    })

  # combine "ms" with "newMS" and save it as "ms"
  ms$timeList <- lapply(seq_along(ms$timeList), function(i) 
                   ms$timeList[[i]] + newMS$timeList[[i]])
  ms$model@times[["to"]] <- to
  ms$outputList <- lapply(seq_along(ms$outputList), function(i) 
                     rbind(ms$outputList[[i]], newMS$outputList[[i]][-1, ]))
  return(ms)
}

