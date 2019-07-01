context("multSim - output methods")

ms <- readRDS(paste0(tempdir(), "/pdmpsimtest/test_MultSim.rda"))

#-------------- tests ----------------

test_that("method 'print' prints something", {
  expect_output(print(ms), "An S3-object of class multSim")
  expect_output(print(ms), "1 2")
  expect_output(print(ms), "outputs exist for all seeds")
})

test_that("method 'summary' prints something", {
  expect_output(summary(ms), "seed = 1")
  expect_output(summary(ms), "Mean")
})
