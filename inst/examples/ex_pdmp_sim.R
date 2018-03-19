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

