#======== todo =================================================================
#t3 I = survival function?
#' @useDynLib pdmpsim, .registration = TRUE
#' @include pdmp_class.R pdmp_methods.R pdmp_accessors.R pdmp_sim.R mjp_class.R mjp_methods.R mjp_accessors.R
#' @name mjpModel-simulation
#' @rdname mjpModel-sim
NULL

##### method sim ####

#' Simulation of a mjpModel object
#'
#' @param obj object of class \code{\link{mjpModel}} or one of its subclasses
#' @param seed an integer or NULL. This makes the result of \code{sim} 
#' reproducible, although random numbers are used during execution. 
#' Simulation with equal seeds will lead to equal results. If seed is set to 
#' NULL, a new one is created from the current time and the process ID.
#' @param outSlot boolean variable. If FALSE, only the result of the simulation 
#' is returned. If TRUE, the whole \code{obj} is returned with the simulation 
#' result stored in slot \code{out}.
#' @param initialize boolean variable. If initialize = TRUE and \code{obj}
#' contains a user-defined initializing function (\code{initfunc}), this 
#' function will be called before the simulation. This can be useful i. e. 
#' to set a random initial value (see examples). If random numbers are 
#' generated within \code{initfunc}, they will be affected by the given 
#' \code{seed}. To avoid this, add \code{set.seed(NULL)} to the function
#' definition of \code{initfunc}.
#' Parameter \code{initialize} defaults to FALSE, because it is 
#' not necessary for simulating piecewise deterministic Markov models. 
#' @param njump number of maximal possible jumps. The default value is 1e+06 
#' and should be sufficient. If the simulation breaks before the end of 
#' simulation time, try to set a higher value for \code{njump}.
#' @param ... optional parameters passed to the solver function.
#'
#' @return The returned value depends on the parameter \code{outSlot}. 
#' If \code{outSlot = TRUE}, the function returns the complete pdmpModel 
#' instance with the simulation result saved in the \code{out} slot.
#' Otherwise, only the simulation result is returned.
#' @note If the result is stored in slot \code{out}, it can get lost if the 
#' value of the other slots is changed via <-. 
#' See \code{\link{mjp-accessors}} for further informations.
#'
#' @example /inst/examples/ex_mjp_sim.R
#' @seealso function \code{\link{multSim}} or \code{\link{multSimCsv}} 
#' for multiple simulations, ... for plot and summary methods of the simulation.
#' @importFrom stats approx
#' @rdname mjpModel-sim
#' @aliases sim,mjpModel-method 
#' @export
setMethod("sim", "mjpModel", function(obj, initialize = FALSE, 
                                       seed = 1,  
                                       njump = 1e+06, outSlot = TRUE, ...) {
  # initialization
  seed <- rep(seed, len = 2)
  set.seed(seed[1])
  if(initialize) {
    obj <- initialize(obj)
  }
  set.seed(seed[2])
  times <- fromtoby(obj@times)
  parms <- obj@parms
  oinit <- obj@init
  #check consistency
  objdim <- length(obj@init) #  number of variables
  ratesdim<-length(obj@ratefunc(t=times[1],x=oinit,parms=obj@parms));
    for (i in 1:ratesdim) {if (length(obj@jumpfunc(times[1],oinit,parms,i)) != objdim) stop("jump function has wrong dimension of output")}
  #actual   
    outa<-.Call("sim_mjp",as.integer(njump),
               as.double(oinit),
               as.double(obj@parms),
               as.double(range(times)),
               obj@jumpfunc,
               obj@ratefunc,parent.frame(),
               NAOK=TRUE,PACKAGE="pdmpsim");
    out<-as.data.frame(cbind(times,apply(X=outa[,-1,drop=FALSE],
                             MARGIN = 2,
                             FUN=function(u) approx(x=outa[,1],y=u,method="const",xout=times,f=0,ties="ordered")$y)))
    colnames(out)<-c("t",names(obj@init))
  obj@out <- out
  if(outSlot) return(invisible(obj))
  else return(invisible(out))
})
