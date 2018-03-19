\dontshow{
  require("simecol")
}
# a model with different initial values and one jumptype
initModel <- pdmpModel(
  descr = "a model with different initial values",
  init = c(f = 0, d = 1),
  initfunc = function(pdmp){ # funktioniert nicht mit "new", immer die gleichen Zufallszahlen!
    set.seed(NULL)
    init(pdmp) <- c("f" = runif(1, min = 1, max = 100), "d" = 1) # assign random initial values
    invisible(pdmp)
  },
  times = c(from = 0, to = 10, by = 0.01),
  dynfunc = function(t, x, parms) c(x["d"]*t, 0),
  ratefunc = function(t, x, parms) 1,
  jumpfunc = function(t, x, parms, jtype) c(-x["f"], (-1)*x["d"])
)
print(init(initModel))
print(head(out(sim(initModel, seed = c(1,2), initialize = TRUE))))
print(head(out(sim(initModel, seed = c(5,2), initialize = TRUE))))


# a pdmp with different jumptypes
jtypePdmp <- pdmpModel(
  descr = "a pdmp with two jumptypes",
  init = c(f = 0, d = 0),
  times = c(from = 0, to = 10, by = 0.01),
  dynfunc = function(t, x, parms) c(x["d"], 0),
  ratefunc = function(t, x, parms) c(1+x["d"], 1-x["d"]),
  jumpfunc = function(t, x, parms, jtype){
    c(0, switch(jtype, x["d"]-1, x["d"]+1))
  }
)
jtypePdmp <- sim(jtypePdmp, seed = 10) # simulate
#dev.off()
#matplot(out(jtypePdmp)[, 1], out(jtypePdmp)[, 2:3], type = "l", lty = 1, col = 1:2,
#        xlab = "time", ylab = "", main = descr(jtypePdmp))
#legend("bottomleft", legend = c("f", "d"), fill = 1:2)


# the same pdmp defined with two discrete variables
discPdmp <- pdmpModel(
  descr = "a pdmp with two discrete variables and two jumptypes",
  init = c(f = 0, d1 = 0, d2 = 0),
  times = c(from = 0, to = 10, by = 0.01),
  dynfunc = function(t, x, parms) c(x["d1"]-x["d2"], 0, 0),
  ratefunc = function(t, x, parms) c(1,1),
  jumpfunc = function(t, x, parms, jtype){
    c(0, switch(jtype, c((x["d1"]-1)^2, x["d2"]),
                c(x["d1"], (x["d2"]-1)^2)))
  }
)
discPdmp <- sim(discPdmp, seed = 10) # simulate
#print(head(out(discPdmp)))

### see the demo files for more sophisticated examples

