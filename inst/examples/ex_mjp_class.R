\dontshow{
  require("simecol")
}

# a mjp with different jumptypes
simplemjp <- mjpModel(
  descr = "a MJP with 2 jumptypes",
  init = c(A = 0, B = 0),
  ratefunc = function(t, x, parms) parms[1:2],
  jumpfunc = function(t, x, parms, jtype){
    x+switch(jtype,c(0,1),c(1,0))
  },
  parms=c(a=0,b=0)
)
simplemjp <- sim(simplemjp, seed = 10) # simulate
plot(simplemjp)

print(head(out(simplemjp)))

# see ?simplePdmp for an explanation of this example
# and ?toggleSwitch for a more sophisticated example

