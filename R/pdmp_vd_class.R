#======== todo =================================================================
#t1 Beispiele verändern (simplePdmp?, mit Initfunc?)
#t2 Warum muss die Dokumentation doppelt sein? Konstruktor anders definieren?
#t3 initialize method schreiben wie in polyPdmpKlasse
#t2 achtung: zur zeit MUSS solver = "lsodar" mitübergeben werden. warum?
#t3 validity method schreiben wie in polyPdmpKlasse (aber mit Inhalt!)
#t1 merge classes documentations

#' @import methods
NULL

#=========== pdmpModel ===============

#' @title Class pdmp_vd_Model - variable dimensions PDMPs
#'
#' @description An S4 class to represent a piecewise deterministic Markov 
#' process (PDMP) with values in variable dimensions.\cr
#' This class is based on the \pkg{simecol} Package and provides a possibility 
#' to simulate piecewise deterministic Markov processes. These processes
#' are described in [Dav84], but at the moment  there are no borders 
#' allowed.
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
#' @example /inst/examples/ex_pdmp_vd_class.R
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
#' @aliases pdmp-vd pdmp_vd_Model PDMP(variable dimensions)
#' @seealso
#' See \code{\link{pdmpModel}} .
#' Class \code{pdmp_vd_Model} provides a method \code{\link{sim}} for simulation, 
#' \code{\link{pdmp-accessors}{accessor functions}} (with names identical to the 
#' slot names) to get or set model parameters, time steps, initial values, the 
#' vectorfields, the transition rates and the solver.
#' \code{\link{multSim}} and \code{\link{multSimCsv}} are not available at the moment
#' 
#' @importFrom simecol simObj
#' @export
setClass("pdmp_vd_Model",
         slots = list(summaryfunc = "function"),
         contains = "pdmpModel")

#' @rdname pdmp_vd_Model-class
#' @slot obj pdmpModel object that is being built. 
#' @slot descr a string containing a short description of the model. 
#' This parameter is optional and only used in plot methods.
#' @slot parms a list with constant model parameters.
#' @slot times vector of time steps or vector with three named values 
#' "from", "to", "by" specifying the simulation time steps. The from-to-by can 
#' be edited with fixParms.
#' @slot  init initial state of the simulation. This is a named vector giving
#' the names of all variables and their start value.
#' @slot discStates a list. For every discrete variable, \code{discStates} contains
#' a vector with all its possible state values. This entry should have the
#' same name as the discrete variable.
#' @slot dynfunc a \code{function(time, variables, parms)} that returns a 
#' vector with odes for every variable. The order and length of the vector 
#' should be the same as in slot "init", discrete variables should have 0 
#' as entry.
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
#' @slot initfunc this parameter can hold an optional function which has a 
#' pdmpModel as only parameter and returnes a (modified) pdmp. 
#' This function is called automatically when a new object is created by 
#' \code{new} or when it is reinitialized by \code{initialize(obj)} or before 
#' starting a simulation with \code{sim(obj, initialize = TRUE)}.
#' @slot summaryfunc is a function which contracts a vector of variable dimension 
#' to a vector of fixed dimension. This is needed for plotting a (multi-)simulation 
#' object from this class.   
#' @slot out NULL or an object of class deSolve. If a simulation is done with 
#' method \code{\link{sim}}, the result will be stored in this parameter.
#' @importFrom deSolve lsodar
#' @exportClass pdmp_vd_Model
#' @export pdmp_vd_Model
pdmp_vd_Model <- function(obj = NULL, 
                      descr = character(0), 
                      dynfunc = function(t, x, parms) 0 * x, 
                      jumpfunc = function(t, x, parms, jtype) x, 
                      ratefunc = function(t, x, parms) c(0),
                      summaryfunc = function(x)x[1],
                      times = c(from = 0, to = 10, by = 1), 
                      init = c(0, 0), 
                      parms = c(0), 
                      discStates = list(0),
                      out = NULL, 
                      solver = "lsodar", 
                      initfunc = NULL) {
  obj <- new("pdmp_vd_Model", dynfunc = dynfunc, jumpfunc = jumpfunc,
             descr = descr, ratefunc = ratefunc, times = times, init = init, 
             parms = parms, initfunc = initfunc, summaryfunc=summaryfunc, 
             solver = solver, out = out,
             discStates = discStates)
  invisible(obj)
}
