#======== todo =================================================================
#t2 more efficient simulation in C 
#t3 I = survival function?

#' @include pdmp_vd_class.R pdmp_vd_methods.R
NULL

##### method sim ####

#' Simulation of a pdmp_vd_Model object
#'
#' @param obj obj of class \code{\link{pdmp_vd_Model}} or one of its subclasses
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
#' @example /inst/examples/ex_pdmp_vd_sim.R
##' @seealso function \code{\link{multSim}} or \code{\link{multSimCsv}} 
##' for multiple simulations, ... for plot and summary methods of the simulation.
#' @aliases sim
#' @importMethodsFrom simecol sim
#' @importFrom simecol fromtoby
#' @importFrom stats rexp
#' @export
setMethod("sim", "pdmp_vd_Model", function(obj, initialize = FALSE, 
                                       seed = 1, outrate = FALSE, 
                                       outSlot = TRUE, ...) {
  
  ### important variables:
  # ξ₁,...,ξₖ = k continous variables (only used in comments)
  # θ = one discrete variable (only used in comments)
  # x = (ξ₁,…,ξₖ,θ) = y[-objdim - 1]. vector of length n
  # y = (ξ₁,…,ξₖ,θ,I), vector of length n+1 whith
  # I = „negcumrate“ = ₀∫ᵗ λ(i₀,φᵢ₀(u,x₀)) du = ₀∫ᵗ Σᵢ ratefunc[i](u,x,parms) du
  # I is used to determine the next jumptime (↔ I == 0) 
  #   and is simulated along with the other variables.

  # initialization
  seed <- rep(seed, len = 2)
  set.seed(seed[1])
  if(initialize) {
    obj <- initialize(obj)
  }
  set.seed(seed[2])
  times <- fromtoby(obj@times)
  parms <- obj@parms
  m<-length(times)
  simf<-function(t0,t1,x){
    objdim <- length(x)
    #dynamics:vector field 
    dfunc <- function(t, y, parms) {
      # dξᵢ/dt = dynfunc[i], dθ/dt = dynfunc[n] = 0,
      list(c(obj@dynfunc(t, x = y[-objdim - 1], parms = parms),
             # dI/dt = Σᵢ ratefunc[i]
             #make sure rates are positive
             sum(pmax(obj@ratefunc(t = t, x = y[-objdim - 1], parms = parms),0))))
    }
    # rootfunc = I (rootfunc == 0 jumpfunc is executed
    rootfunc <- function(t, y, parms) y[objdim + 1]
    # inity = initial state for y = (obj@init, I₀) with I₀ ~ -exp
    inity <- c(x, -rexp(n = 1))
    names(inity) <- c(names(x), "pdmpsim:negcumrate")
    
    # call of ode-solver (default: lsodar)
    if (outrate) {
      lout <- do.call(obj@solver, list(y = inity, times = c(t0,t1),
                                func = dfunc, initpar = parms, 
                                rootfunc = rootfunc,
                                nroot = 1))
    }
    else {
      lout <- do.call(obj@solver, list(y = inity, times = c(t0,t1),
                                      func = dfunc, initpar = parms, 
                                      rootfunc = rootfunc,
                                      nroot = 1))[,-objdim-2]
    }
    return(list(rf=.hasSlot(lout,iroot) , x=lout[2,-1],t=lout[2,1]))
  }
  
  xi<-init
  out<-vector("list",m)
  out[[1]]<-list(t=times[1],x=init)
  #loop
  for(i in 1:(m-1))
  {t0<-times[i]
  t1<-times[i+1]
  while (t0<t1) {
    ow<-simf(t0,t1,xi)
    t0<-ow$t
    if(ow$rf){
      w <- obj@ratefunc(t = t0, x = ow$x, parms = parms)
      #sample the jump type
      #make sure pobabilities are positive
      jtype <- sample.int(n = length(w), size = 1, prob = pmax(w,0))
      # jumpfunc(jtype) = next state after the jump
      xi<-obj@jumpfunc(t = t0, x = ow$x, 
                  parms = parms, jtype = jtype)
      }
    else {
      xi<-ow$x
      }
  }
  out[[i+1]]<-list(t=times[i+1],x=xi)
  }
  obj@out<-out
  if(outSlot) return(invisible(obj))
  else return(invisible(out))
})
