\dontshow{
  require("simecol")
}

data("OÖK") # load object of class pdmpBorder

#### simulations

# simulate and plot the result
plot(sim(OÖK, seed = 1))

# simulate and plot the result with random seed
plot(sim(OÖK, seed = NULL))

# simulating again with seed = 1 leads to the same result as before
plot(sim(OÖK, seed = 1))

#### be careful about slot out!

out(OÖK) # NULL, because results have not been stored

simIB <- sim(OÖK) # simulation results are stored in out
head(out(simIB))

init(OÖK) <- c(height = 0, velocity = 10)
head(out(OÖK)) # NULL, because slot init has changed

simIB <- sim(OÖK, outSlot = FALSE)
str(simIB) # only the simulation result, the pdmpBorder object is lost

### an example with initialize = TRUE

initModel <- pdmpBorder(
  descr = "a model with random initial values",
  init = c(energy = 1, area = 1, modi = 2),
  initfunc = function(pdmpB){
    set.seed(NULL) # necessary to be not affected by parameter 'seed'
    init(pdmp) <- c("energy" = runif(1, min = 1, max = 9), "area" = 1, "modi" = 2) 
    invisible(pdmpB)
  },
  times = c(from = 0, to = 100, by = 0.1),
  borroot = function(t, x, parms){return(c(x[1]-9))}, 
  terroot = function(t, x, parms){return(c(x[1]))},
  discStates = list(area = (c(1, 2, 3)), modi = c(1, 2, 3)),
  dynfunc = function(t, x, parms) {
    denergy <- with(as.list(c(x, parms)),switch(area, switch(modi, a_11*energy - β, a_12 - β*energy, a_13*energy - β), 
                                                switch(modi, a_21*energy - β, a_22 - β*energy, a_23*energy - β), 
                                                switch(modi, a_31*energy - β, a_32 - β*energy, a_33*energy - β))) 
    return(c(denergy, 0, 0))
  },
  ratefunc = function(t, x, parms) {
    return(with(as.list(c(x, parms)), c(switch(area, switch(modi, c(κ112, κ113, s_12*energy, s_13*energy), c(κ123, κ121, 0, 0), c(κ131, κ132, 0, 0)),
                                               switch(modi, c(κ212, κ213, s_23*energy, s_21*energy), c(κ223, κ221, 0, 0), c(κ231, κ232, 0, 0)),
                                               switch(modi, c(κ312, κ313, s_31*energy, s_32*energy), c(κ323, κ321, 0, 0), c(κ331, κ332, 0, 0))))))
  },
  jumpfunc = function(t, x, parms, jtype) {
    return(with(as.list(c(x,parms)), c(x[1], switch(jtype, switch(x[2], 1, 2, 3), switch(x[2], 1, 2, 3), 
                                                    switch(x[2], 2 ,3 ,1), switch(x[2], 3, 2, 1)),
                                       switch(jtype, switch(x[3], 2, 3, 1), switch(x[3], 3, 1, 2), switch(x[3], 1, 2, 3), switch(x[3], 1, 2, 3)))))
  },  
  
  borderfunc = function(t, x, parms){ return(with(as.list(c(x,parms)), c(x[1:2], 1)))
  },

)
print(init(initModel))
print(head(out(sim(initModel, seed = 2, initialize = TRUE))))
print(head(out(sim(initModel, seed = 5, initialize = TRUE))))

