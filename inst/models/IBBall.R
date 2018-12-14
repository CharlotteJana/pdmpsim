
IBBall <-pdmpBorder(
  descr = "increasing bouncing ball, just borderjumps",
  parms = list(a=-5, b=-1.1),
  init = c(height = 0, velocity = 10),
  discStates = list(),
  borroot = function(t, x, parms){return(c(x[1]))},
  terroot = function(t, x, parms){return(c(x[1]-20))},
  times = seq(from = 0, to = 25, by = 0.01),
  dynfunc =  function(t, x, parms) {
    df <- with(as.list(c(x, parms)), c(velocity, a))
  return(c(df))
  },
  ratefunc = function(t, x, parms) {
    0
  },
  jumpfunc = function(t, x, parms) {return(with(as.list(c(x, parms)), c(df)))},
  borderfunc = function(t, x, parms){return(with(as.list(c(x, parms)), c(height = 0, velocity = b* velocity)))
})

# load it and plot a simulation:
data("IBBall")
plot(sim(IBBall))
