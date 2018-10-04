#======== todo =================================================================
#t1 Beispiele verändern 
#t2 Warum muss die Dokumentation doppelt sein? Konstruktor anders definieren?
#t3 add tau-leap method from other packages


#' @import methods
#' @name mjpModel-class
NULL

#=========== mjpModel ===============

#' @title Class mjpModel
#'
#' @description An S4 class to represent a  Markov jump  
#' process (MJP).\cr
#' This class is based on the \pkg{simecol} Package and provides a possibility 
#' to simulate  Markov jump processes. 
#'
#' @slot descr a string containing a short description of the model. 
#' This slot is optional and only used in plot methods.
#' @slot parms a list with constant model parameters.
#' @slot times vector of time steps or vector with three named values "from", 
#' "to", "by" specifying the simulation time steps. The from-to-by can be 
#' edited with fixParms. 
#' @slot init initial state of the simulation. This is a named vector giving
#' the names of all variables and their start value.
#' @slot ratefunc a \code{function(t, x, parms)} that returns a vector with
#' transition rates from the actual state to another state. Only non zero rates 
#' are given. The length of the returned vector determines the number of 
#' different jumptypes. 
#' @slot jumpfunc a \code{function(t, x, parms, jtype)} that returns the next 
#' state the process will jump to. This state depends on parameter
#' \code{jtype}. The number of possible \code{jtypes} is determined by function
#' \code{ratefunc}. The value for \code{jtype} will be chosen randomly during
#' simulation, depending ot the rates given in \code{ratefunc}.
#' @slot solver a function or a character string specifying the numerical 
#' algorithm used. At the moment it is ignored since only the direct algorithm of Gillespie ("Gdirect") [Gil77] is implemented.  
#' @slot initfunc this slot can hold an optional function which has a mjpModel
#' as only parameter and returnes an object of class \code{mjpModel}. 
#' This function is called automatically when a new object is created by 
#' \code{new} or when it is reinitialized by \code{initialize(obj)} 
#' or before starting a simulation with \code{sim(obj, initialize = TRUE)}.
#' @slot out NULL or an object of class deSolve. If a simulation is done with 
#' method \code{\link{sim}}, the result will be stored in this slot.
#'
#' @example /inst/examples/ex_mjp_class.R
#'
#' @references
#' \tabular{ll}{
#' \eqn{\,}{ }[Gil77] \tab Gillespie, Daniel T. (1977). "Exact Stochastic Simulation of Coupled Chemical Reactions". \cr
#' \emph{The Journal of Physical Chemistry} 81 (25): 2340–2361. doi:10.1021/j100540a008.\cr
#'  }
#'
#' @aliases mjp mjpmodel MJP
#' @seealso
#' See \code{\link{SIRstoch}} and \code{\link{KendallBD}} for two examples
#' that have a detailed documentation explaining every slot.
#' Class \code{mjpModel} provides a method \code{\link{sim}} for simulation, 
#' \code{\link{mjpModel-accessors}{accessor functions}} (with names identical to the 
#' slot names) to get or set model parameters, time steps, initial values, the 
#' vectorfields, the transition rates and the solver.
#' See \code{\link{multSim}} and \code{\link{multSimCsv}} to perform multiple 
#' simulations for a \code{mjpModel}.
#' @importFrom simecol simObj
#' @export
setClass("mjpModel",
         slots = list(descr = "character", 
                      parms = "numericOrlist", 
                      init = "numeric", 
                      times = "numeric", 
                      jumpfunc = "function", 
                      ratefunc = "function", 
                      initfunc = "functionOrNULL",
                      solver="character"),
         contains = "simObj")

#' @rdname mjpModel-class
#' @param obj pdmpModel object that is being built. 
#' @param descr a string containing a short description of the model. 
#' This parameter is optional and only used in plot methods.
#' @param parms a list with constant model parameters.
#' @param times vector of time steps or vector with three named values 
#' "from", "to", "by" specifying the simulation time steps. The from-to-by can 
#' be edited with fixParms.
#' @param init initial state of the simulation. This is a named vector giving
#' the names of all variables and their start value.
#' @param ratefunc a \code{function(t, x, parms)} that returns a vector with
#' transition rates from the actual state to another state. Only non zero rates 
#' are given. The length of the returned vector determines the number of 
#' different jumptypes.
#' @param jumpfunc a \code{function(t, x, parms, jtype)} that returns the next 
#' discrete state the process will jump to. This state depends on parameter
#' \code{jtype}. The number of possible \code{jtypes} is determined by function
#' \code{ratefunc}. The value for \code{jtype} will be chosen randomly during
#' simulation, depending ot the rates given in \code{ratefunc}.
#' @param solver a function or a character string specifying the numerical 
#' algorithm used.At the moment it is ignored since only the "direct" algorithm of Gillespie ("Gdirect")  is implemented.  
#' @param initfunc this parameter can hold an optional function which has a 
#' pdmpModel as only parameter and returnes a (modified) pdmp. 
#' This function is called automatically when a new object is created by 
#' \code{new} or when it is reinitialized by \code{initialize(obj)} or before 
#' starting a simulation with \code{sim(obj, initialize = TRUE)}.
#' @param out NULL or an object of class deSolve. If a simulation is done with 
#' method \code{\link{sim}}, the result will be stored in this parameter.
#' @export
mjpModel <- function(obj = NULL, 
                      descr = character(0), 
                      jumpfunc = function(t, x, parms, jtype) x, 
                      ratefunc = function(t, x, parms) c(0), 
                      times = c(from = 0, to = 10, by = 1), 
                      init = c(0, 0), 
                      parms = c(0), 
                      out = NULL, 
                      solver = "Gdirect", 
                      initfunc = NULL) {
  obj <- new("mjpModel",  jumpfunc = jumpfunc,
             descr = descr, ratefunc = ratefunc, times = times, init = init, 
             parms = parms, initfunc = initfunc, solver = solver, out = out)
  invisible(obj)
}
