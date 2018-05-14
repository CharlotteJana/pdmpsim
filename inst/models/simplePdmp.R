## the code used to generate this model:

simplePdmp <- pdmpModel(
    descr = "A simple PDMP",
    init = c(f = 0, d = 0),
    discStates = list(d = c(-1, 0, 1)),
    times = c(from = 0, to = 10, by = 0.01),
    dynfunc = function(t, x, parms) c(x["d"], 0),
    ratefunc = function(t, x, parms) c(1+x["d"], 1-x["d"]),
    jumpfunc = function(t, x, parms, jtype){
         c(0, switch(jtype, x["d"]-1, x["d"]+1))
    })

## load it and plot a simulation:
data("simplePdmp")
plot(sim(simplePdmp))
