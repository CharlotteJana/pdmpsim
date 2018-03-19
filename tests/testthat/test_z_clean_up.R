#------------ remove all files that were created during tests ----------

setwd(system.file("testdata", package = "pdmpsim", mustWork = TRUE))
unlink('*')