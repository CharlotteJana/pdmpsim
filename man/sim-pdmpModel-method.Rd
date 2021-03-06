% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdmp_sim.R
\docType{methods}
\name{sim,pdmpModel-method}
\alias{sim,pdmpModel-method}
\alias{sim}
\title{Simulation of a pdmpModel object}
\usage{
\S4method{sim}{pdmpModel}(obj, initialize = FALSE, seed = 1,
  outrate = FALSE, nroot = 1e+06, outSlot = TRUE, ...)
}
\arguments{
\item{obj}{obj of class \code{\link{pdmpModel}} or one of its subclasses}

\item{initialize}{boolean varialbe. If initialize = TRUE and \code{obj}
contains a user-defined initializing function (\code{initfunc}), this 
function will be called before the simulation. This can be useful i. e. 
to set a random initial value (see examples). If random numbers are 
generated within \code{initfunc}, they will be affected by the given 
\code{seed}. To avoid this, add \code{set.seed(NULL)} to the function
definition of \code{initfunc}.
Parameter \code{initialize} defaults to FALSE, because it is 
not necessary for simulating piecewise deterministic Markov models.}

\item{seed}{an integer or NULL. This makes the result of \code{sim} 
reproducible, although random numbers are used during execution. 
Simulation with equal seeds will lead to equal results. If seed is set to 
NULL, a new one is created from the current time and the process ID.}

\item{outrate}{boolean variable. During simulation, the survival function is 
simulated too, because it is needed to determine the next jump time. 
Setting outrate = TRUE will return all simulated values including the 
survival function \code{I}. If outrate = FALSE, the values for \code{I} 
will not be returned.}

\item{nroot}{number of maximal possible jumps. The default value is 1e+06 
and should be sufficient. If the simulation breaks before the end of 
simulation time, try to set a higher value for \code{nroot}.}

\item{outSlot}{boolean variable. If FALSE, only the result of the simulation 
is returned. If TRUE, the whole \code{obj} is returned with the simulation 
result stored in slot \code{out}.}

\item{...}{optional parameters passed to the solver function 
(e.g. hmax for lsoda).}
}
\value{
The returned value depends on the parameter \code{outSlot}. 
If \code{outSlot = TRUE}, the function returns the complete pdmpModel 
instance with the simulation result saved in the \code{out} slot.
Otherwise, only the simulation result is returned.
}
\description{
Simulation of a pdmpModel object
}
\note{
If the result is stored in slot \code{out}, it can get lost if the 
value of the other slots is changed via <-. 
See \code{\link{pdmp-accessors}} for further informations.
}
\examples{
data("simplePdmp") # load object of class pdmpModel

#### simulations

# simulate and plot the result
plot(sim(simplePdmp, seed = 1))

# simulate and plot the result with random seed
plot(sim(simplePdmp, seed = NULL))

# simulating again with seed = 1 leads to the same result as before
plot(sim(simplePdmp, seed = 1))

#### be careful about slot out!

out(simplePdmp) # NULL, because results have not been stored

simplePdmp <- sim(simplePdmp) # simulation results are stored in out
head(out(simplePdmp))

init(simplePdmp) <- c(f = 5, d = 1)
head(out(simplePdmp)) # NULL, because slot init has changed

simplePdmp <- sim(simplePdmp, outSlot = FALSE)
str(simplePdmp) # only the simulation result, the pdmpModel object is lost

### an example with initialize = TRUE

initModel <- pdmpModel(
  descr = "a model with random initial values",
  init = c(f = 0, d = 1),
  discStates = list(d = c(-1, 1)),
  initfunc = function(pdmp){
    set.seed(NULL) # necessary to be not affected by parameter 'seed'
    init(pdmp) <- c("f" = runif(1, min = 1, max = 100), "d" = 1) 
    invisible(pdmp)
  },
  times = c(from = 0, to = 10, by = 0.01),
  dynfunc = function(t, x, parms) c(x["d"]*t, 0),
  ratefunc = function(t, x, parms) 1,
  jumpfunc = function(t, x, parms, jtype) c(-x["f"], (-1)*x["d"])
)
print(init(initModel))
print(head(out(sim(initModel, seed = 2, initialize = TRUE))))
print(head(out(sim(initModel, seed = 5, initialize = TRUE))))
}
\seealso{
function \code{\link{multSim}} or \code{\link{multSimCsv}} 
for multiple simulations, ... for plot and summary methods of the simulation.
}
