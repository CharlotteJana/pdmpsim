context("multSim - output methods")

pdmp <- pdmpModel(
  descr = "a simple model",
  init = c(f = 10, d = 1),
  times = c(from = 0, to = 10, by = 0.01),
  dynfunc = function(t, x, parms) c(x["d"]*t, 0),
  ratefunc = function(t, x, parms) 1,
  jumpfunc = function(t, x, parms, jtype) c(-x["f"], (-1)*x["d"])
)

ms <- multSim(pdmp, seeds = 1:10)

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

test_that("method 'plot' plots correctly for multSim objects", {
  skip_if_not_installed("plot3D")
  plot(ms)
  testplot <- function(){
    invisible(plot(ms))
    dev.off()
  }
  #vdiffr::expect_doppelganger("plot-multSim", plot(ms), path = "")
})

test_that("method 'hist' plots correctly for multSim objects", {
  expect_error(hist(ms, t = c(5, 10)))
  testplot <- function() hist(ms, t = 10)
  vdiffr::expect_doppelganger("hist-multSim", testplot, path = "")
})

test_that("method 'density' plots correctly for multSim objects", {
  testplot <- function() invisible(density(ms, t = c(5, 10)))
  vdiffr::expect_doppelganger("density-multSim", testplot, path = "")
})