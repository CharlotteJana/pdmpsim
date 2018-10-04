\dontshow{
  require("simecol")
}

data("simplePdmp") # load object of class pdmpModel

#### simulations

# simulate and plot the result
plot(sim(simplePdmp, seed = 1))

# simulate and plot the result with random seed
plot(sim(simplePdmp, seed = NULL))

# simulating again with seed = 1 leads to the same result as before
plot(sim(simplePdmp, seed = 1))

#### be careful about slot out!

out(simplePdmp) # NULL, because results have not been stored

simplePdmp <- sim(simplePdmp) # simulation results are stored in out
head(out(simplePdmp))

init(simplePdmp) <- c(f = 5, d = 1)
head(out(simplePdmp)) # NULL, because slot init has changed

simplePdmp <- sim(simplePdmp, outSlot = FALSE)
str(simplePdmp) # only the simulation result, the pdmpModel object is lost

### an example with initialize = TRUE

initModel <- pdmpModel(
  descr = "a model with random initial values",
  init = c(f = 0, d = 1),
  initfunc = function(pdmp){
    set.seed(NULL) # necessary to be not affected by parameter 'seed'
    init(pdmp) <- c("f" = runif(1, min = 1, max = 100), "d" = 1) 
    invisible(pdmp)
  },
  times = c(from = 0, to = 10, by = 0.01),
  dynfunc = function(t, x, parms) c(x["d"]*t, 0),
  ratefunc = function(t, x, parms) 1,
  jumpfunc = function(t, x, parms, jtype) c(-x["f"], (-1)*x["d"])
)
print(init(initModel))
print(head(out(sim(initModel, seed = 2, initialize = TRUE))))
print(head(out(sim(initModel, seed = 5, initialize = TRUE))))

