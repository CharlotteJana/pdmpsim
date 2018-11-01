#======== todo =================================================================
#t1 Beispiele verändern (simplePdmp?, mit Initfunc?)
#t2 Warum muss die Dokumentation doppelt sein? Konstruktor anders definieren?
#t3 initialize method schreiben wie in polyPdmpKlasse
#t2 achtung: zur zeit MUSS solver = "lsodar" mitübergeben werden. warum?
#t3 validity method schreiben wie in polyPdmpKlasse (aber mit Inhalt!)

#' @import methods
NULL

#=========== pdmpModel ===============

#' @title Class pdmpModel
#'
#' @description An S4 class to represent a piecewise deterministic markov 
#' process (PDMP).\cr
#' This class is based on the \pkg{simecol} Package and provides a possibility 
#' to simulate piecewise deterministic markov processes. These processes
#' are described in [Zei09] and [Ben+15]. There are some restrictions compared 
#' to the original definition given in [Dav84]: In our case there are no borders 
#' allowed and the number of continous variables should be independent of the 
#' state (of the discrete variable) of the process.
#'
#' @slot descr a string containing a short description of the model. 
#' This slot is optional and only used in plot methods.
#' @slot parms a list with constant model parameters.
#' @slot times vector of time steps or vector with three named values "from", 
#' "to", "by" specifying the simulation time steps. The from-to-by can be 
#' edited with fixParms.
#' @slot init initial state of the simulation. This is a named vector giving
#' the names of all variables and their start value.
#' @slot discStates a list. For every discrete variable, \code{discStates} 
#' contains a vector with all its possible state values. This entry should have 
#' the same name as the discrete variable.
#' @slot dynfunc a \code{function(time, variables, parms)} that returns a vector
#' with odes for every variable. The order and length of the vector should be 
#' the same as in slot "init", discrete variables should have 0 as entry.
#' @slot ratefunc a \code{function(t, x, parms)} that returns a vector with
#' transition rates from the actual state to another state. Only non zero rates 
#' are given. The length of the returned vector determines the number of 
#' different jumptypes. 
#' @slot jumpfunc a \code{function(t, x, parms, jtype)} that returns the next 
#' discrete state the process will jump to. This state depends on parameter
#' \code{jtype}. The number of possible \code{jtypes} is determined by function
#' \code{ratefunc}. The value for \code{jtype} will be chosen randomly during
#' simulation, depending ot the rates given in \code{ratefunc}.
#' @slot solver a function or a character string specifying the numerical 
#' algorithm used, e.g. "lsoda", "rk4" or "euler" from package deSolve. 
#' The default solver is "lsodar".
#' @slot initfunc this slot can hold an optional function which has a pdmpModel
#' as only parameter and returnes an object of class \code{pdmpModel}. 
#' This function is called automatically when a new object is created by 
#' \code{new} or when it is reinitialized by \code{initialize(obj)} 
#' or before starting a simulation with \code{sim(obj, initialize = TRUE)}.
#' @slot out NULL or an object of class deSolve. If a simulation is done with 
#' method \code{\link{sim}}, the result will be stored in this slot.
#'
#' @example /inst/examples/ex_pdmp_class.R
#'
#' @references
#' \tabular{ll}{
#' \eqn{\,}{ }[Dav84] \tab Davis, M. H. (1984). Piecewise-deterministic Markov 
#' processes: A general class of \cr
#'  \tab non-diffusion stochastic models. \emph{Journal of the Royal 
#'  Statistical Society. Series B} \cr
#'  \tab \emph{(Methodological)}, 353-388. \cr
#' [Zei09] \tab S. Zeiser. \emph{Classical and Hybrid Modeling of Gene 
#' Regulatory Networks}. 2009. \cr
#' [Ben+15]\eqn{\,\,\,\,}{    } \tab Benaïm, M., Le Borgne, S., Malrieu, F., 
#' & Zitt, P. A. (2015). Qualitative properties \cr
#' \tab of certain piecewise deterministic Markov processes. 
#' In \emph{Annales de l'Institut Henri} \cr
#' \tab \emph{Poincaré, Probabilités et Statistiques} (Vol. 51, No. 3, 
#' pp. 1040-1075). Institut \cr
#' \tab Henri Poincaré. \cr
#'  }
#'
#' @aliases pdmp pdmpmodel PDMP
#' @seealso
#' See \code{\link{simplePdmp}} and \code{\link{toggleSwitch}} for two examples
#' that have a detailed documentation explaining every slot.
#' Class \code{pdmpModel} provides a method \code{\link{sim}} for simulation, 
#' \code{\link{pdmp-accessors}{accessor functions}} (with names identical to the 
#' slot names) to get or set model parameters, time steps, initial values, the 
#' vectorfields, the transition rates and the solver.
#' See \code{\link{multSim}} and \code{\link{multSimCsv}} to perform multiple 
#' simulations for a \code{pdmpModel}.
#' @importFrom simecol simObj
#' @export
setClass("pdmpModel",
         slots = list(descr = "character", 
                      parms = "numericOrlist", 
                      init = "numeric", 
                      discStates = "list",
                      times = "numeric",
                      dynfunc = "function", 
                      jumpfunc = "function", 
                      ratefunc = "function", 
                      initfunc = "functionOrNULL"),
         contains = "simObj")

#' @rdname pdmpModel-class
#' @param obj pdmpModel object that is being built. 
#' @param descr a string containing a short description of the model. 
#' This parameter is optional and only used in plot methods.
#' @param parms a vector or list with constant model parameters.
#' @param times vector of time steps or vector with three named values 
#' "from", "to", "by" specifying the simulation time steps. The from-to-by can 
#' be edited with fixParms.
#' @param init initial state of the simulation. This is a named vector giving
#' the names of all variables and their start value.
#' @param discStates a list. For every discrete variable, \code{discStates} contains
#' a vector with all its possible state values. This entry should have the
#' same name as the discrete variable.
#' @param dynfunc a \code{function(time, variables, parms)} that returns a 
#' vector with odes for every variable. The order and length of the vector 
#' should be the same as in slot "init", discrete variables should have 0 
#' as entry.
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
#' algorithm used, e.g. "lsoda", "rk4" or "euler" from package deSolve. 
#' The default solver is "lsodar".
#' @param initfunc this parameter can hold an optional function which has a 
#' pdmpModel as only parameter and returnes a (modified) pdmp. 
#' This function is called automatically when a new object is created by 
#' \code{new} or when it is reinitialized by \code{initialize(obj)} or before 
#' starting a simulation with \code{sim(obj, initialize = TRUE)}.
#' @param out NULL or an object of class deSolve. If a simulation is done with 
#' method \code{\link{sim}}, the result will be stored in this parameter.
#' @importFrom deSolve lsodar
#' @export
pdmpModel <- function(obj = NULL, 
                      descr = character(0), 
                      dynfunc = function(t, x, parms) 0 * x, 
                      jumpfunc = function(t, x, parms, jtype) x, 
                      ratefunc = function(t, x, parms) c(0), 
                      times = c(from = 0, to = 10, by = 1), 
                      init = c(0, 0), 
                      parms = list(), 
                      discStates = list(0),
                      out = NULL, 
                      solver = "lsodar", 
                      initfunc = NULL) {
  obj <- new("pdmpModel", dynfunc = dynfunc, jumpfunc = jumpfunc,
             descr = descr, ratefunc = ratefunc, times = times, init = init, 
             parms = parms, initfunc = initfunc, solver = solver, out = out,
             discStates = discStates)
  invisible(obj)
}
