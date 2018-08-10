dontshow{
  require("simecol")
}

data("KendallBD") # load object of class pdmpModel

#### simulations

# simulate and plot the result
plot(sim(KendallBD, seed = 1))

# simulate and plot the result with random seed
plot(sim(KendallBD, seed = NULL))

# simulating again with seed = 1 leads to the same result as before
plot(sim(KendallBD, seed = 1))

#### be careful about slot out!

out(KendallBD) # NULL, because results have not been stored

simBD <- sim(kendallBD) # simulation results are stored in out
head(out(simBD))

init(KendallBD) <- c(N = 10)
head(out(KendallBD)) # NULL, because slot init has changed

simBD <- sim(KendallBD, outSlot = FALSE)
str(simBD) # only the simulation result, the pdmpModel object is lost


