#======== todo =================================================================
#s1 toggleSwitch: references
#t2: SIRstoch: implement
#tv2 toggleSwitch: k01 und k10 zusammenfassen?

#######################################################################

#' A simple PDMP
#'
#' This is a simple example for a piecewise deterministic markov process
#' defined as \code{\link{pdmpModel}}. It is included to the package for 
#' demonstration purposes and is used in some unit tests and function 
#' examples.
#' @slot parms There are no parameters for this model.
#' @slot init  There is one continous variable \code{f} with initial value 0
#' and one discrete variable \code{d} initial value 0.
#' @slot discStates The discrete variable \code{d} has codomain \{-1, 0, 1\}.
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
#' leads to a probability of \eqn{\frac{1}{2}}{½} to have a jumptype of type 
#' 1 or 2.
#' @slot times The simulations will start at time \code{t = 0} and end at 
#' \code{t = 10} with step length 0.01.
#' @format An object of class \code{\link{pdmpModel}}.
#' @seealso \code{\link{toggleSwitch}} for a more sophisticated example of a 
#' \code{pdmpModel} and \code{\link{pdmpModel}}
#' for the formal description of the S4 class.
#' @example inst/models/simplePdmp.R
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
#' @slot init  There are two continous variables \eqn{f_A}{fA} and \eqn{f_B}{fB}
#' which describe the concentration of gene products from gene A and gene B, 
#' respectivly. Both have initial value 0.5. The two discrete variables 
#' \eqn{d_A}{dA} and \eqn{d_B}{dB} describe the expression state of gene A and 
#' gene B and have initial value 1, which means that both genes are not blocked.
#' @slot discStates The discrete variables \eqn{d_A}{dA} and \eqn{d_B}{dB} both
#' have codomain \{0, 1\} where 0 stands for "Gene A/B is blocked" and 1 
#' stands for "Gene A/B is unblocked".
#' @slot dynfunc The dynamic of the continous variable \eqn{f_A}{fA} depends on 
#' the state of discrete variable \eqn{d_A}{dA}. In case \eqn{d_A = 0}{dA = 0} 
#' it is given by \eqn{\frac{df_A}{dt} = -b_A \cdot f_A}{dfA/dt = -bA⋅fA} 
#' describing an exponential decay of the concentration of gene product \code{A}. 
#' If \eqn{d_A = 1}{dA = 1}, there is an additional term of linear growth leading 
#' to the ODE \eqn{\frac{df_A}{dt} = -b_A \cdot f_A + a_A}{dfA/dt = -bA⋅fA + aA}.
#' Both formulas can be combined to \eqn{\frac{df_A}{dt} = -b_A \cdot f_A + a_A 
#' \cdot d_A}{dfA/dt = -bA⋅fA + aA⋅dA}. Accordingly, the dynamic of dB is given 
#' as \eqn{\frac{df_B}{dt} = -b_B \cdot f_B + a_B \cdot d_B}{dfB/dt = -bB⋅fB + 
#' aB⋅dB}.
#' @slot jumpfunc There are two jumptypes. If the jump is of type 1, only the 
#' value of \eqn{d_B}{dB} is changed (from 0 to 1 or 1 to 0, respectivly). 
#' All other variables remain unchanged. The second jumptype changes only the 
#' value of \eqn{d_A}{dA}.
#' @slot ratefunc A vector of length two determining the probability of a jump 
#' being of type 1 or 2. In this model, a molecule of gene product A can act as 
#' repressor and impede the gene expression of gene B. The rates for the first 
#' jumptype (which describes a change in the gene expression of gene B) are 
#' therefore \eqn{k_{10A} \cdot fA}{k10A⋅fA} for the change from the unblocked 
#' (\eqn{d_B = 1}{dB = 1}) to the blocked (\eqn{d_B = 0}{dB = 0}) state and 
#' \eqn{k_{01A}}{k01A} for the change from the blocked to the unblocked state 
#' of gene B. The rates for the second jumptype are generated in an analogous 
#' way because gene A is repressed by a molecule of gene product B and the 
#' second jumptype describes a change in the gene expression of gene A.
#' @slot parms There are are a number of parameters that appear in the dynamics 
#' and rates of the process, namely \eqn{b_A, b_B, a_A, a_B, k_{01A}, k_{10A},
#' k_{01B}}{bA, bB, aA, aB, k01A, k10A, k01B} and \eqn{k_{10B}}{k10B}. 
#' The values of the parameters in this example are artificial and are not based
#' on real data.
#' @slot times The simulations will start at time \code{t = 0} and end at 
#' \code{t = 10} with step length 0.01.
#' @format An object of class \code{\link{pdmpModel}}.
#' @seealso \code{\link{simplePdmp}} for an easier example of a \code{pdmpModel}
#' and \code{\link{pdmpModel}}
#' for the formal description of the S4 class.
#' @example inst/models/toggleSwitch.R
"toggleSwitch"


#######################################################################

#' Kendalls birth-and-death process
#'
#' This is a simple example for a markov jump process
#' defined as \code{\link{mjpModel}}. It is included to the package for 
#' demonstration purposes.
#' @slot parms birth and death rate.
#' @slot init  There is one integer  variable \code{N} with initial value 1.
#' @slot jumpfunc There are two jumptypes. Births  jump from \code{N} to 
#' \code{N + 1}, deaths from \code{N} to \code{N - 1}. 
#' @slot ratefunc A vector of length two determining the probability of a jump 
#' being a birth or a death. 
#' @slot times The simulations will start at time \code{t = 0} and end at 
#' \code{t = 10} with step length 0.01.
#' @format An object of class \code{\link{mjpModel}}.
#' @seealso \code{\link{SIRstoch}} for a more sophisticated example of a 
#' \code{mjpModel} and \code{\link{mjpModel-class}}
#' for the formal description of the S4 class.
#' @example inst/models/KendallBD.R
"KendallBD"


#######################################################################

#' Stochastic SIR dynamics
#'
#' This is a more complicated example for a markov jump process
#' defined as \code{\link{mjpModel}}. It is included to the package for 
#' demonstration purposes.
#' @slot parms infection (\code{beta}) and recovery  rate (\code{gamma}).
#' @slot init  There are three integer  variables \code{S,I,R} with initial value (10,1,0).
#' @slot jumpfunc There are two jumptypes. infections  jump from \code{(S,I,R)} to 
#' \code{(S-1,I+1,R)}, recoveries from \code{(S,I,R)} to \code{(S,I-1,R+1)}. 
#' @slot ratefunc A vector of length two determining the probability of a jump 
#' being a recovery or infection. 
#' @slot times The simulations will start at time \code{t = 0} and end at 
#' \code{t = 10} with step length 0.01.
#' @format An object of class \code{\link{mjpModel}}.
#' @seealso \code{\link{KendallBD}} for a  simpler example of a 
#' \code{mjpModel} and \code{\link{mjpModel-class}}
#' for the formal description of the S4 class.
#' @example inst/models/SIRstoch.R
"SIRstoch"

#######################################################################

#' Increased bouncing ball
#' 
#' This is a minimal fictitious example, of a PDMP jumping at borders 
#' \code{\link{pdmpBorder}}, where no stochastic jumps exist, so that it is easy 
#' to see the functionality of borders.
#' Here the minimum border 0 ist the ground where the velocity of the ball
#' rises and therefore the height of each hop increases up to a second 
#' fixed border (20), where the process stops.
#' @slot init  There ist one continous variable, which describes the velocity of 
#' the ball and no discrete variable.
#' @slot discStates there are no discstates.
#' @slot dynfunc The dynamic of the continous variable depends on 
#' the velocity at the last jump (borderjump or stochastic jump).
#' @slot jumpfunc 
#' @slot ratefunc 
#' @slot parms There are are a number of parameters that appear in the dynamics 
#' and  no change rates of the process.
#' @slot times The simulations will start at time \code{t = 0} and end at the time, where the termination terminating value is reached.
#' @slot borroot It has dimension one and replaces the ground (=minimum)
#' 
#' 
#' @slot terroot Every value, which forces the process to terminate is stored in the vector terroot.
#' In this example it has dimension one and is the fixes value of 20 which forces the precess to terminate.
#' @slot borderfunc The simulations will start at time \code{t = 0} and end at time where the termination terminating value is reached.
#' 
#' @format An object of class \code{\link{pdmpBorder}}.
#' @seealso \code{\link{OÖK}} for an easier example of a \code{pdmpBorder}
#' and \code{\link{pdmpBorder}}
#' for the formal description of the S4 class.
#' @example inst/models/IBBall.R
"IBBall"

#######################################################################

#' Autecological PDMP
#' 
#' This is a more complicated example, of a PDMP jumping at borders 
#' \code{\link{pdmpBorder}}, where we consider stochastic jumps  
#' as well as fixes jumps at borders.
#' The model describes the energy of an individual, which can live in 
#' different areas an be in different modi, which lead to a differant 
#' dynamic and between which the individual switches
#' stochastically and fixed, in case of reaching a border.
#' The minimum border 0 means that the individual dies and therefor the 
#' process terminates.
#' The Maximum border forces a jump into another modus.
#' @slot init There is onecontinous variable \eqn{dz_1}{dz}
#' which describe the energy of the considered individuum and two discrete variables 
#' \eqn{dz_2}{dz} and \eqn{dz_3}{dz}, which describe the modus and area of the individuum.
#' @slot discStates One discrete variable \eqn{dz_3}{dz} models the modus it has codomain \{1, 2, 3\} where 1 means 
#' that the individuum is in search modus, 2 means that the individuum is eating ans 3 means that it is in flight modus.
#' The other one \eqn{dz_2}{dz} models the area whith codomain \{1, 2, 3\}.
#' @slot dynfunc The dynamic of the continous variable depends on 
#' the velocity at the last jump (borderjump or stochastic jump), and the new discrete state.
#' @slot jumpfunc There are four jumptypes, the first two just change the modus, and the other ones just the area.
#' @slot ratefunc  A vector of length four determining the probability of a jump 
#' being of jumptype 1, 2, 3 or 4 (two possible modusjumps and two possible areajumps).
#' The rates of modus change are modelled constant but the rates of changing the area are dependig on the engery value.
#' Here, the rates areachanging are zero iff the modus is unequal to 1, which means that the model assumes, that the individuum
#' just changes the area iff it is in searching modus.
#' @slot parms There are are a number of parameters that appear in the dynamics 
#' and rates of the process. First of all there are the parameters of decrease and increase of energy in the different states \eqn{β, a_11, a_21, a_31, a_22, a_12, a_32, a_23, a_13, a_33}.
#' Then we have area change rates, which are depending on the engergy and the recent area (=searching individuum) \eqn{s_12, s_21, s_31, s_13, s_23, s_32} 
#' and we have the modus change rates, differing in each area on the recent modus, because of different food resources, competitors and hunters 
#' \eqn{κ112, κ113, κ131, κ132, κ121, κ123, κ212, κ213, κ231, κ232, κ221, κ223, κ312, κ313, κ331, κ332, κ321, κ323}. 
#' The values of the parameters in this example are artificial and are not based on real data.
#' @slot times The simulations will start at time \code{t = 0} and end at the terminatin borderjump.
#' @slot borroot Every value, which forces the process to jump into a fixed state is stored in the vector borroot.
#' In this example it has dimension one and is the engery maximum of 2.
#' @slot terroot Every value, which forces the process to terminate is stored in the vector terroot.
#' In this example it has dimension one and is the energy minimum of 0.
#' @slot borderfunc The simulations will start at time \code{t = 0} and end at 
#' \code{t = 10} with step length 0.01.
#' @format An object of class \code{\link{pdmpBorder}}.
#' @seealso \code{\link{IBBall}} for an easier example of a \code{pdmpBorder}.
#' @example inst/models/OÖK.R
"OÖK"

