context("multSim - output methods")

data("simplePdmp")
suppressMessages(
  ms <- multSim(simplePdmp, seeds = 1:10)
)

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
