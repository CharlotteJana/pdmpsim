## the code used to generate this model:

SIRstoch <- mjpModel(
    descr = "stochastic SIR epidemic",
    init = c(S=10,I=1,R=0),
    times = c(from = 0, to = 10, by = 0.01),
    ratefunc = function(t, x, parms) c(parms[1]*x[1]*x[2],parms[2]*x[2]),
    jumpfunc = function(t, x, parms, jtype){
         x+switch(jtype,c(-1,1,0),c(0,-1,1))
    },
    parms=c(beta=1,gamma=1))

## load it and plot a simulation:
data("SIRstoch")
plot(sim(SIRstoch))
