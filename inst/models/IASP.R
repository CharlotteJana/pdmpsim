## the code used to generate this model:
IASP <- pdmp_vd_Model(
  descr = "a PDMP model for imigration and aging",
  init = c(0),
  times = c(from = 0, to = 1e2, by = 1),
  dynfunc = function(t, x, parms) {return(0*x+1)},
  ratefunc = function(t, x, parms) {return(parms[[1]])},
  jumpfunc = function(t, x, parms, jtype) c(0,x),
  summaryfunc= function(x) c(mean=mean(x),size=length(x)),
  parms = c(lambda=1)
)

## load it and plot a simulation:
data("IASP")
plot(sim(IASP))
