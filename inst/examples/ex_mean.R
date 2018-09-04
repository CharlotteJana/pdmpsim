data(simplePdmp)
sim <- multSim(simplePdmp, seeds = 1:10)
mean(sim)

# handling NA values:
sim$outputList[[1]][1,2] <- NA
sim$outputList <- c(sim$outputList, NA) # will be removed before calculation
head(mean(sim))
head(mean(sim, na.rm = TRUE))

# Methods mean.multSim and mean.multSimCsv
# can lead to slightly different results:
sim2 <- multSimCsv(simplePdmp, seeds = 1:10)
all.equal(mean(sim), mean(sim2))
