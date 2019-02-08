#======== todo =================================================================
#t1 imported packages: stats, reshape2, prodlim

#' pdmpsim: simulate PDMPs
#'
#' The core of this package is the S4 class \code{\link{pdmpModel}} that has the 
#' aim to represent piecewise deterministic markov processes (PDMPs) in R. If a 
#' PDMP is implemented as \code{pdmpModel}, it can be simulated with method 
#' \code{\link{sim}}. The package provides another class named 
#' \code{\link{multSim}} to store multiple simulations and some methods for 
#' plotting and analysing the simulation results. Additionally the generator of 
#' a PDMP can be calculated with method \code{\link{generator}}. 
#' 
#' @section PDMPs:
#' A PDMP is a stochastic process that consists of continous variables and 
#' discrete variables. Discrete variables are simulated like a usual discrete 
#' markov chain with finite state space. The jump rates for the different states 
#' are defined in slot \code{ratefunc} where as the state of the variable after 
#' a jump is generated using slot \code{jumpfunc}. The continous variables 
#' evolve according to ODEs that are defined in slot \code{dynfunc}. These ODEs 
#' usually depend on the states of the discrete variables. The number of the 
#' continous variables is however fixed and does not change during the 
#' simulation. This is one restriction compared to the more general definition 
#' of PDMPs given in [Dav84]. The other restriction concernes borders for the 
#' continous variables which are not yet implemented in the simulation algorithm. 
#' See [Zei09] and [Ben+15] for an introduction of PDMPs with definitions that 
#' match with the implementation of this package.
#' 
#' @section Provided Methods:
#' There is a bunch of methods that can be used to analyse the simulation 
#' results. A single simulation stored in slot \code{out} of class 
#' \code{pdmpModel} can be visualised with \code{\link{plot}} and summarized 
#' with \code{\link{summarise}}. To store multiple simulations in a conventient 
#' way, use \code{\link{multSim}} or \code{\link{multSimCsv}}.
#' The latter is only needed for simulations generating big data that can not be 
#' loaded to the working memory anymore. Method \code{multSimCsv} stores the 
#' results in csv files and returnes a class with links to the corresponding 
#' files instead of returning the simulations themselves. 
#' 
#' @section Imported Packages:
#' The following packages are needed for package \pkg{pdmpsim} to work: \cr
#' For solving the ODEs during the simulation, package \pkg{deSolve} is required. 
#' To store multiple simulations as \code{csv} files and work with them without 
#' loading them into the working memory, package \pkg{LaF} is needed. 
#' Package \pkg{Deriv} is only used in function \code{\link{generator}}. 
#' Most of the plot methods are based on \pkg{ggplot2} and some additional 
#' \code{ggplot2} packages.
#'  
#' @references
#' \tabular{ll}{
#' \eqn{\,}{ }[Dav84] \tab Davis, M. H. (1984). Piecewise-deterministic Markov processes: 
#'  A general class of \cr
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
#' @name pdmpsim
#' @docType package
NULL
