#======== todo =================================================================

#' @include pdmp_class.R
NULL

#' Generator
#' 
#' Compute the generator of a PDMP. The generator is defined as follows:
#' Let \eqn{X_t}{Xₜ} be a PDMP with statespace \eqn{K \times D}{K x D} where 
#' \eqn{K \subset R^k}{K ⊂ ℝᵏ} and \eqn{D} is the state space for the discrete 
#' variable. Let furthermore \eqn{\varphi^s(t,i,z)}{φˢ(t,i,z)} be the dynamics 
#' for the continous variables, \eqn{s = 1,...,k} and 
#' \eqn{\Lambda_{ij}(z)}{Λᵢⱼ(z)} be the transition rates 
#' \eqn{i \rightarrow j}{i → j} for \eqn{i,j \in D}{i,j ϵ D}. 
#' Let \eqn{z^*}{z*} be the new continous values after a jump from 
#' \eqn{x := (i,z)} to \eqn{j}. The generator for a function 
#' \eqn{f: K \times D \rightarrow R^k}{f: K x D → ℝᵏ} lying in its domain is 
#' defined as \deqn{Q(f)(t,x) = Q(f)(t,i,z) := \sum_{s = 1}^{k} \varphi^s(t,i,z) 
#' \frac{\partial f(i,z)}{\partial z_s} + \sum_{j \in D} 
#' \Lambda_{ij}(z)(f(j,z^*) - f(i,z)).}{Q(f)(t,x) = Q(f)(t,i,z) := Σ φˢ(t,i,z) 
#' ∂f(i,z)/∂zₛ + Σ Λᵢⱼ(z)(f(j,z*) - f(i,z))}
#' \ifelse{latex}{}{where the first sum goes from s = 1 to k and the second 
#' sums over all j ϵ D.}
#'
#' @param obj an object of class pdmpModel or one of its subclasses
#' @return The generator \code{Q} of \code{obj} as defined above. This is a
#'   function which takes as argument a single function \code{f}. The arguments
#'   of function \code{f} have to be the same as the variables of the process
#'   \eqn{X_t}{Xₜ} and should therefore have the same names and the same order
#'   as the variables given in \code{init(obj)}. The resulting function
#'   \code{Q(f)} is a function with parameters t, x where t is the time value
#'   and x is a named vector with the same names (in the same order) as in
#'   \code{init(obj)}.
#' @examples
#' data("simplePdmp")
#' g <- function(d, f) d*f
#' generator(simplePdmp)(g)(t = 10, x = c("d" = -1, "f" = 10))
#' 
#' # comparison with theoretic solution:
#' Qg_theoretic <- function(d, f) d^2-2*d*f
#' f_values <- seq(from = 0, to = 4, by = 0.01)
#' Qg_method <- function(d, f) sapply(f, function(fi) 
#'  generator(simplePdmp)(g)(t = 5, x = c("d" = d, "f" = fi))
#' )
#' identical(Qg_theoretic(-1, f_values), Qg_method(-1, f_values))
#' plot(f_values, Qg_theoretic(1, f_values))
#' lines(f_values, Qg_method(1, f_values), col = "red", lwd = 3)
#' @export
setGeneric("generator", function(obj) standardGeneric("generator"))

#' @rdname generator
setMethod("generator", signature(obj = "pdmpModel"), function(obj) {
  
  if(class(obj) == "mjpModel")
    stop("Method 'generator' is not implemented for objects of class 'mjpModel'.")
  if(class(obj) == "pdmpBorder")
    stop("Method 'generator' is not implemented for objects of class 'pdmpBorder'.")
  if(!requireNamespace("Deriv", quietly = TRUE)) 
    stop("Method 'generator' depends on package 'Deriv'. Please install it.")
  
  function(f){
      function(t, x, parms = obj@parms) {
      n <- length(obj@init)
      nam <- names(obj@init)
      nj <- length(obj@ratefunc(t, x, parms)) # number of jumptypes
      dyn <- obj@dynfunc(t, x, parms)
      xold <- as.list(x)

      # sum over continuous states
      s1 <- sum(vapply(1:n, 
                       function(i) dyn[i]*do.call(Deriv::Deriv(f,nam[i]), xold),
                       numeric(1)))

      # sum over possible new discrete states
      s2 <- sum(vapply(1:nj, 
        function(jtype){
          xnew <- as.list(obj@jumpfunc(t, x, parms, jtype))
          names(xnew) <- nam
          obj@ratefunc(t, x, parms)[jtype]*(do.call(f,xnew)-do.call(f,xold))
        },
        numeric(1)))

      return(s1 + s2)
    }
  }
})

