## the code used to generate this model:

KendallBD <- mjpModel(
    descr = "Kendall's birth-death process",
    init = c(N=10),
    times = c(from = 0, to = 10, by = 0.01),
    ratefunc = function(t, x, parms) parms[1:2]*x,
    jumpfunc = function(t, x, parms, jtype){
         x+switch(jtype,1,-1)
    },
    parms=c(b=1,d=1))

## load it and plot a simulation:
data("KendallBD")
plot(sim(KendallBD))
