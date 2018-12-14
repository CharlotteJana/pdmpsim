#======== todo =================================================================
#t2 Warum muss die Dokumentation doppelt sein? Konstruktor anders definieren?- bei mir auch- pdmppoly nein..
#t2 achtung: zur zeit MUSS solver = "lsodar" mitübergeben werden. warum?

#' @import methods
NULL

#=========== pdmpModel ===============

#' @title Class pdmpBorder
#'
#' @description An S4 class to represent a piecewise deterministic markov 
#' process with Borders (PDMP).\cr
#' This class is based on the \pkg{pdmpsim} which itselve is based on \pkg{simecol} Package and provides a possibility 
#' to simulate piecewise deterministic markov processes with borders. These processes
#' are described in [Zei09] and [Ben+15]. It's closed to the original definition given in [Dav84], but one Restriction left: 
#' The number of continous variables should be independent of the 
#' state (of the discrete variable) of the process.
#' The class is based on the \code{\link[pdmpsim]{pdmpModel}} class of package
#' \pkg{pdmpsim} but introduces additional slots
#' @slot borderfunc a \code{function(t, x, parms)} that returns the new state after reaching a borroot.
#' @slot borroot a \code{function(t, x, parms)} that returns a zero for each continous 
#' variable reaching a border where the process forces a jump (maximum).
#' @slot terroot a \code{function(t, x, parms)} that returns a zero for each contionous 
#' variable reaching a border where the process forces a termination (minimum).
#'
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
#' @aliases pdmp pdmpborder PDMP
#' @seealso
#' Class \code{pdmpModel} provides a method \code{\link{sim}} for simulation, 
#' \code{\link{pdmpBorder-accessors}{accessor functions}}
#' @importFrom methods new
#' @export
setClass("pdmpBorder",
         slots = list(borderfunc = "function", 
                      borroot = "function", 
                      terroot = "function"),
         contains = "pdmpModel")

#' @rdname pdmpBorder-class
#' 
#' @param borderfunc a \code{function(t, x, parms)} that returns the new state after reaching a borroot.
#' @param borroot a \code{function(t, x, parms)} that returns a zero for each continous 
#' variable reaching a border where the process forces a jump (maximum).
#' @param terroot a \code{function(t, x, parms)} that returns a zero for each contionous 
#' variable reaching a border where the process forces a termination (minimum).
#' @importFrom deSolve lsodar
#' @export
pdmpBorder <- function(obj = NULL, 
                       descr = character(0), 
                       dynfunc, 
                       jumpfunc, 
                       ratefunc, 
                       borderfunc = function(t, z, parms) z,
                       borroot = function(t, z, parms) NULL, 
                       terroot = function(t, z, parms) NULL,
                       times = c(from = 0, to = 10, by = 1), 
                       init = c(0, 0), 
                       parms = c(0), 
                       discStates = list(0),
                       out = NULL, 
                       solver = "lsodar", 
                       initfunc = NULL) {
  obj <- new(Class = "pdmpBorder", dynfunc = dynfunc, jumpfunc = jumpfunc, borderfunc = borderfunc,
             descr = descr, ratefunc = ratefunc, borroot = borroot, terroot = terroot, times = times, init = init, 
             parms = parms, initfunc = initfunc, solver = solver, out = out,
             discStates = discStates)
  invisible(obj)
}
