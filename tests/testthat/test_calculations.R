#### todo
#t3 test with NAs in simulations

context("calculations")

ms <- readRDS(paste0(tempdir(), "/pdmpsimtest/test_MultSim.rda"))
msCsv <- loadMultSimCsv(paste0(tempdir(), "/pdmpsimtest/test_MultSimCsv.rda"))

#-------------- tests ----------------

test_that("method 'mean' and 'moments(..., order = 1)' give equal results", {
  means <- mean(ms)
  meansCsv <- mean(msCsv)
  moments <- moments(ms, order = 1)
  
  expect_equal(means, dplyr::select(moments, -order))
  expect_equal(means, meansCsv, tolerance = 1e-07)
  # 'm.' and 'm.Csv' are not equal because 'multSimCsv' only stores 7 digits.
})