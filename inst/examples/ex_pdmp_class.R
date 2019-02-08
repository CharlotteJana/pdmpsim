
# a pdmp with different jumptypes
simplePdmp <- pdmpModel(
  descr = "a PDMP with 2 jumptypes",
  init = c(f = 0, d = 0),
  times = c(from = 0, to = 10, by = 0.01),
  discStates = list(d = -1:1),
  dynfunc = function(t, x, parms) c(x["d"], 0),
  ratefunc = function(t, x, parms) c(1+x["d"], 1-x["d"]),
  jumpfunc = function(t, x, parms, jtype){
    c(0, switch(jtype, x["d"]-1, x["d"]+1))
  }
)
simplePdmp <- sim(simplePdmp, seed = 10) # simulate
plot(simplePdmp)

# the same pdmp defined with two discrete variables
discPdmp <- pdmpModel(
  descr = "a PDMP with 2 discrete variables and 2 jumptypes",
  init = c(f = 0, d1 = 0, d2 = 0),
  discStates = list(d1 = 0:1, d2 = 0:1),
  times = c(from = 0, to = 10, by = 0.01),
  dynfunc = function(t, x, parms) c(x["d1"]-x["d2"], 0, 0),
  ratefunc = function(t, x, parms) c(1,1),
  jumpfunc = function(t, x, parms, jtype){
    c(0, switch(jtype, c((x["d1"]-1)^2, x["d2"]),
                c(x["d1"], (x["d2"]-1)^2)))
  }
)
discPdmp <- sim(discPdmp, seed = 10) # simulate
print(head(out(discPdmp)))

# see ?simplePdmp for an explanation of this example
# and ?toggleSwitch for a more sophisticated example

