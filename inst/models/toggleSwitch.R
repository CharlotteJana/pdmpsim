## the code used to generate this model:

toggleSwitch <- pdmpModel(
    descr = "Toggle Switch with two Promotors",
    parms = list(bA = 0.5, bB = 0.5, aA = 2, aB = 4, 
                 k01A = 0.5, k10A = 2, k01B = 0.3, k10B = 3),
    init = c(fA = 0.5, fB = 0.5, dA = 1.0, dB = 1.0),
    discStates = list(dA = c(0, 1), dB = c(0, 1)),
    times = c(from = 0, to = 100, by = 0.01),
    dynfunc = function(t, x, parms) {
       df <- with(as.list(c(x, parms)), c(-bA*fA + aA*dA, -bB*fB + aB*dB))
       return(c(df, 0, 0))
    },
    ratefunc = function(t, x, parms) {
       return(with(as.list(c(x, parms)), c(switch(dB+1, k01B, k10B*fA),
                                           switch(dA+1, k01A, k10A*fB))))
    },
    jumpfunc = function(t, x, parms, jtype){
       return(with(as.list(c(x, parms)), c(fA, fB, switch(jtype,
                                                          c(dA, 1-dB),
                                                          c(1-dA, dB)))))
    })

## load it and plot a simulation:
data("toggleSwitch")
plot(sim(toggleSwitch))
