\dontshow{
  require("simecol")
}

# an imigration-aging process
IASP <- pdmpModel(
  descr = "a PDMP model for imigration and aging",
  init = c(0),
  times = c(from = 0, to = 1e2, by = 1),
  dynfunc = function(t, x, parms) 0*x+1,
  ratefunc = function(t, x, parms) parms[1],
  jumpfunc = function(t, x, parms, jtype) c(0,x),
  summaryfunc= function(x) c(mean=mean(x),size=length(x)),
  parms = c(lambda=1)
)
iasp_sim <- sim(IASP, seed = 10) # simulate
plot(iasp_sim)


