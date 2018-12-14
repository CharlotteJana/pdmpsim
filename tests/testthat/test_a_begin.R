#======== todo =================================================================
#t1 tests ohne setwd und ohne dass der ordner testdata existieren muss

#-------- simulate pdmps and save the results for testing -----------

setwd(system.file("testdata", package = "pdmpsim", mustWork = TRUE))
unlink('test*') # removes all files from working directory

simplePdmp <- pdmpModel(
  descr = "a simple pdmp",
  init = c(f = 0, d = 0),
  times = c(from = 0, to = 10, by = 0.1),
  discStates = list(d = -1:1),
  dynfunc = function(t, x, parms) c(x["d"], 0),
  ratefunc = function(t, x, parms) c(1+x["d"], 1-x["d"]),
  jumpfunc = function(t, x, parms, jtype){
    c(0, switch(jtype, x["d"]-1, x["d"]+1))
  }
)

suppressMessages(try({
  multSimCsv(simplePdmp, seeds = 1:5, prefix = "test")
  saveRDS(multSim(simplePdmp, seeds = 1:5), file = "test_MultSim.rda")
}, silent = TRUE))
