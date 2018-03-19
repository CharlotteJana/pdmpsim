#### todo
# testen mit NAs in den Simulationen

context("calculations")
setwd(system.file("testdata", package = "pdmpsim", mustWork = TRUE))
ms <- readRDS("test_MultSim.rda")
msCsv <- loadMultSimCsv("test_MultSimCsv.rda")

#-------------- tests ----------------

test_that("method 'mean' and 'moments(..., order = 1)' give equal results", {
  means <- mean(ms)
  meansCsv <- mean(msCsv)
  moments <- moments(ms, order = 1)
  
  expect_equal(means, moments)
  expect_equal(means, meansCsv, tolerance = 1e-07)
  # 'm.' and 'm.Csv' are not equal because 'multSimCsv' only stores 7 digits.
})