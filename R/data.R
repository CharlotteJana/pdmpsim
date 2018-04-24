##### Todo #####
#t1 toggleSwitch: references

#######################################################################

#' A simple PDMP
#'
#' This is a simple example for a piecewise deterministic markov process
#' defined as \code{\link{pdmpModel}}. It is included to the package for 
#' demonstration purposes #' and is used in some unit tests and function 
#' examples.
#' @slot parms There are no parameters for this model.
#' @slot init  There is one continous variable \code{f} with initial value 0
#' and one discrete variable \code{d} with codomain \{-1, 0, 1\} and initial value 0.
#' @slot dynfunc The continous variable \code{f} evolves as linear function
#' \eqn{f(t) = t} if \code{d = 1},
#' as \eqn{f(t) = -t} if \code{d = -1} and is constant zero if \code{d = 0}. 
#' Its dynamic can therefore be described as \eqn{\frac{df}{dt} = d}{df/dt = d}.
#' @slot jumpfunc There are two jumptypes. The first jumps from \code{d} to 
#' \code{d - 1}, the second from \code{d} to \code{d + 1}. Both reset \code{f} 
#' to zero.
#' @slot ratefunc A vector of length two determining the probability of a jump 
#' being of type 1 or 2. If \code{d = -1}, it has value \code{(0, 2)} forcing 
#' the jumptype to be of type 2. The same takes place for \code{d = 1}: 
#' \code{ratefunc} returnes \code{(2, 0)} and the jumptype is therefore always 
#' of type 1. In case \code{d = 0}, \code{ratefunc} returnes \code{(1, 1)} which
#' leads to a probability of \eqn{\frac{1}{2}}{½} to have a jumptype of type 1 or 2.
#' @slot times The simulations will start at time \code{t = 0} and end at 
#' \code{t = 10} with step length 0.01.
#' @format An object of class \code{\link{pdmpModel}}.
#' @seealso \code{\link{toggleSwitch}} for a more sophisticated example of a 
#' \code{pdmpModel} and \code{\link{pdmpModel}}
#' for the formal description of the S4 class.
#' @examples
#' ## the code used to generate this model:
#' simplePdmp <- pdmpModel(
#'     descr = "A simple PDMP",
#'     init = c(f = 0, d = 0),
#'     times = c(from = 0, to = 10, by = 0.01),
#'     dynfunc = function(t, x, parms) c(x["d"], 0),
#'     ratefunc = function(t, x, parms) c(1+x["d"], 1-x["d"]),
#'     jumpfunc = function(t, x, parms, jtype){
#'          c(0, switch(jtype, x["d"]-1, x["d"]+1))
#'     })
#'
#' ## load it and plot a simulation:
#' data("simplePdmp")
#' plot(sim(simplePdmp))
"simplePdmp"

#######################################################################

#' The toggle switch model
#'
#' This is an example for a piecewise deterministic markov process defined as 
#' \code{\link{pdmpModel}}. It models a gene regulation mechanism that is called 
#' toggle switch. This mechanism describes two genes A and B whose gene products
#' are repressors to the other gene, i. e. the gene product of A blocks the gene 
#' expression of gene B and vice versa. This model is an example for a PDMP with 
#' two discrete and two continous variables. It is included to the package for 
#' demonstration purposes and is used in some unit tests and function examples.
#'
#' @slot init  There are two continous variables \code{fA} and \code{fB} which 
#' describe the concentration of gene products from gene A and gene B, 
#' respectivly. Both have initial value 0.5. The two discrete variables 
#' \code{dA} and \code{dB} describe the expression state of gene A and gene B. 
#' They have a codomain \{0, 1\} where 0 stands for "blocked" and 1 stands for 
#' "unblocked" and have the initial value 1.
#' @slot dynfunc The dynamic of the continous variable \code{fA} depends on the 
#' state of discrete variable \code{dA}. In case \code{dA = 0} it is given by 
#' \eqn{\frac{df_A}{dt} = -b_A \cdot f_A}{dfA/dt = -bA⋅fA} describing an 
#' exponential decay of the concentration of gene product \code{A}. If 
#' \code{dA = 1}, there is an additional term of linear growth leading to the 
#' ODE \eqn{\frac{df_A}{dt} = -b_A \cdot f_A + a_A}{dfA/dt = -bA⋅fA + aA}.
#' Both formulas can be combined to \eqn{\frac{df_A}{dt} = -b_A \cdot f_A + a_A 
#' \cdot d_A}{dfA/dt = -bA⋅fA + aA⋅dA}. Accordingly, the dynamic of dB is given 
#' as \eqn{\frac{df_B}{dt} = -b_B \cdot f_B + a_B \cdot d_B}{dfB/dt = -bB⋅fB + 
#' aB⋅dB}.
#' @slot jumpfunc There are two jumptypes. If the jump is of type 1, only the 
#' value of dB is changed (from 0 to 1 or 1 to 0, respectivly). All other 
#' variables remain unchanged. The second jumptype changes only the value of 
#' \code{dA}.
#' @slot ratefunc A vector of length two determining the probability of a jump 
#' being of type 1 or 2. In this model, a molecule of gene product A can act as 
#' repressor and impede the gene expression of gene B. The rates for the first 
#' jumptype (which describes a change in the gene expression of gene B) are 
#' therefore \eqn{k10 \cdot fA}{k10⋅fA} for the change from the unblocked 
#' (dB = 1) to the blocked (dB = 0) state and k01 for the change from the 
#' blocked to the unblocked state of gene B. The rates for the second jumptype 
#' are generated in an analogous way because gene A is repressed by a molecule 
#' of gene product B and the second jumptype describes a change in the gene 
#' expression of gene A.
#' @slot parms There are are a number of parameters that appear in the dynamics 
#' and rates of the process, namely \code{bA, bB, aA, aB, k01A, k10A, k01B} and 
#' \code{k10B}. The values of the parameters in this example are artificial and 
#' are not based on real data.
#' @slot times The simulations will start at time \code{t = 0} and end at 
#' \code{t = 10} with step length 0.01.
#' @format An object of class \code{\link{pdmpModel}}.
#' @seealso \code{\link{simplePdmp}} for an easier example of a \code{pdmpModel}
#' and \code{\link{pdmpModel}}
#' for the formal description of the S4 class.
#' @examples
#' ## the code used to generate this model:
#' toggleSwitch <- pdmpModel(
#'     descr = "Toggle Switch with two Promotors",
#'     parms = list(bA = 0.1, bB = 1, aA = 2, aB = 4, k01A = 0.5, k10A = 2, k01B = 1/3, k10B = 3),
#'     init = c(fA = 0.5, fB = 0.5, dA = 1.0, dB = 1.0),
#'     times = c(from = 0, to = 10, by = 0.01),
#'     dynfunc = function(t, x, parms) {
#'        df <- with(as.list(c(x, parms)), c(-bA*fA + aA*dA, -bB*fB + aB*dB))
#'        return(c(df, 0, 0))
#'     },
#'     ratefunc = function(t, x, parms) {
#'        return(with(as.list(c(x, parms)), c(switch(dB+1, k01B, k10B*fA),
#'                                            switch(dA+1, k01A, k10A*fB))))
#'     },
#'     jumpfunc = function(t, x, parms, jtype){
#'        return(with(as.list(c(x, parms)), c(fA, fB, switch(jtype,
#'                                                           c(dA, 1-dB),
#'                                                           c(1-dA, dB)))))
#'     })
#'
#' ## load it and plot a simulation:
#' data("toggleSwitch")
#' plot(sim(toggleSwitch))
"toggleSwitch"
