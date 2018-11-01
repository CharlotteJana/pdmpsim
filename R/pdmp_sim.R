#======== todo =================================================================
#t3 I = survival function?

#' @include pdmp_class.R pdmp_methods.R
NULL

##### method sim ####

#' Simulation of a pdmpModel object
#'
#' @param obj obj of class \code{\link{pdmpModel}} or one of its subclasses
#' @param seed an integer or NULL. This makes the result of \code{sim} 
#' reproducible, although random numbers are used during execution. 
#' Simulation with equal seeds will lead to equal results. If seed is set to 
#' NULL, a new one is created from the current time and the process ID.
#' @param outSlot boolean variable. If FALSE, only the result of the simulation 
#' is returned. If TRUE, the whole \code{obj} is returned with the simulation 
#' result stored in slot \code{out}.
#' @param initialize boolean varialbe. If initialize = TRUE and \code{obj}
#' contains a user-defined initializing function (\code{initfunc}), this 
#' function will be called before the simulation. This can be useful i. e. 
#' to set a random initial value (see examples). If random numbers are 
#' generated within \code{initfunc}, they will be affected by the given 
#' \code{seed}. To avoid this, add \code{set.seed(NULL)} to the function
#' definition of \code{initfunc}.
#' Parameter \code{initialize} defaults to FALSE, because it is 
#' not necessary for simulating piecewise deterministic Markov models. 
#' @param outrate boolean variable. During simulation, the survival function is 
#' simulated too, because it is needed to determine the next jump time. 
#' Setting outrate = TRUE will return all simulated values including the 
#' survival function \code{I}. If outrate = FALSE, the values for \code{I} 
#' will not be returned.
#' @param nroot number of maximal possible jumps. The default value is 1e+06 
#' and should be sufficient. If the simulation breaks before the end of 
#' simulation time, try to set a higher value for \code{nroot}.
#' @param ... optional parameters passed to the solver function 
#' (e.g. hmax for lsoda).
#'
#' @return The returned value depends on the parameter \code{outSlot}. 
#' If \code{outSlot = TRUE}, the function returns the complete pdmpModel 
#' instance with the simulation result saved in the \code{out} slot.
#' Otherwise, only the simulation result is returned.
#' @note If the result is stored in slot \code{out}, it can get lost if the 
#' value of the other slots is changed via <-. 
#' See \code{\link{pdmp-accessors}} for further informations.
#'
#' @example /inst/examples/ex_pdmp_sim.R
#' @seealso function \code{\link{multSim}} or \code{\link{multSimCsv}} 
#' for multiple simulations, ... for plot and summary methods of the simulation.
#' @aliases sim
#' @importMethodsFrom simecol sim
#' @importFrom simecol fromtoby
#' @importFrom stats rexp
#' @export
setMethod("sim", "pdmpModel", function(obj, initialize = FALSE, 
                                       seed = 1, outrate = FALSE, 
                                       nroot = 1e+06, outSlot = TRUE, ...) {
  
  ### important variables:
  # ξ₁,...,ξₖ = k continous variables (only used in comments)
  # θ = one discrete variable (only used in comments)
  # x = (ξ₁,…,ξₖ,θ) = y[-objdim - 1]. vector of length n
  # y = (ξ₁,…,ξₖ,θ,I), vector of length n+1 whith
  # I = „negcumrate“ = ₀∫ᵗ λ(i₀,φᵢ₀(u,x₀)) du = ₀∫ᵗ Σᵢ ratefunc[i](u,x,parms) du
  # I is used to determine the next jumptime (↔ I == 0) 
  #   and is simulated along with the other variables.

  # initialization
  suppressWarnings(
    seed <- rep(seed, len = 2)
  )
  set.seed(seed[1])
  if(initialize) {
    obj <- initialize(obj)
  }
  set.seed(seed[2])
  times <- fromtoby(obj@times)
  parms <- obj@parms
  objdim <- length(obj@init) # = n = continous variables + discrete variables

  # func = rhs of ode system dy/dt
  #      = list (dξ₁/dt, dξ₂/dt, …, dξₙ/dt, dθ/dt, dI/dt)
  func <- function(t, y, parms) {
    # dξᵢ/dt = dynfunc[i], dθ/dt = dynfunc[n] = 0,
    list(c(obj@dynfunc(t, x = y[-objdim - 1], parms = obj@parms),
           # dI/dt = Σᵢ ratefunc[i]
           sum(obj@ratefunc(t = t, x = y[-objdim - 1], parms = obj@parms))))
  }


  # rootfunc = I (rootfunc == 0 → eventfunc will be executed)
  rootfunc <- function(t, y, parms) y[objdim + 1]

  # eventfunc returns c(next state after a jump, I) where I ~ -exp
  eventfunc <- function(t, y, parms) {
    # w = ratefunc(ξₜ,θₜ)
    w <- obj@ratefunc(t = t, x = y[-objdim - 1], parms = obj@parms)
    # jtype = random number, choosed according to transition measure w
    jtype <- sample.int(n = length(w), size = 1, prob = w)
    # jumpfunc(jtype) = next state after the jump
    return(c(obj@jumpfunc(t = t, x = y[-objdim - 1], 
                          parms = obj@parms, jtype = jtype),
             # I is set as negative exponentially distributed random number
             -rexp(n = 1)))
  }

  # events: this is needed for solver lsodar
  events <- list(func = eventfunc, root = TRUE, rootfunc = rootfunc)

  # inity = initial state for y = (obj@init, I₀) with I₀ ~ -exp
  inity <- c(obj@init, -rexp(n = 1))
  names(inity) <- c(names(obj@init), "pdmpsim:negcumrate")

  # call of ode-solver (default: lsodar)
  if (outrate) {
    out <- do.call(obj@solver, list(y = inity, times = times,
                                    func = func, initpar = obj@parms, 
                                    events = events, rootfunc = rootfunc,
                                    nroot = nroot, ...))
  } else {
    out <- do.call(obj@solver, list(y = inity, times = times,
                                    func = func, initpar = obj@parms, 
                                    events = events, rootfunc = rootfunc,
                                    nroot = nroot, ...))[, -objdim - 2]
  }
  class(out) <- c("deSolve", "matrix")
  obj@out <- out

  if(outSlot) return(invisible(obj))
  else return(invisible(out))
})
