### Todo
#t2 Anmerkung: Die auskommentierten Tests arbeiten nicht mit ggplot2.
#t2 Das Testen funktioniert in diesen Fällen nicht, ich weiß aber nicht, warum
#t2 Beim plotStats-Test wird NULL ausgegeben. Warum?

context("plot methods")
  
pdmp <- pdmpModel(
  descr = "a simple model",
  init = c(f = 10, d = 1),
  times = c(from = 0, to = 10, by = 0.01),
  parms = c(alpha = 3),
  dynfunc = function(t, x, parms) c(3*x["d"]*t, 0),
  ratefunc = function(t, x, parms) 1,
  jumpfunc = function(t, x, parms, jtype) c(-x["f"], (-1)*x["d"])
)

suppressMessages(
  ms <- multSim(pdmp, seeds = 1:10)
)

#-------------- tests ----------------


# test_that("method 'plot' plots correctly for pdmpmodel", {
#   expect_error(plot(pdmp))
#   pdmp <- sim(pdmp, seed = 1, outSlot = TRUE)
#   testplot <- function() invisible(plot(pdmp))
#   vdiffr::expect_doppelganger("plot-pdmp", testplot, path = "")
# })

test_that("method 'plot' plots correctly for multSim objects", {
  skip_on_cran()
  testplot <- plot(ms, discPlot = "line")
  vdiffr::expect_doppelganger("plot-line-multSim", testplot, path = "")
})

test_that("method 'plot' plots correctly for multSimData objects", {
  skip_on_cran()
  msd <- getMultSimData(ms)
  testplot <- plot(msd, discPlot = "smooth")
  vdiffr::expect_doppelganger("plot-smooth-multSimData", testplot, path = "")
})


test_that("method 'plotStats' plots correctly for multSimData objects",{
  skip_on_cran()
  msd <- getMultSimData(ms)
  testplot <- function() plotStats(msd, vars = "f", funs = dplyr::funs(min, max, mean))
  vdiffr::expect_doppelganger("plotStats", testplot(), path = "")
})

test_that("method 'plotSeeds' plots correctly for multSimData objects", {
  skip_on_cran()
  msd <- getMultSimData(ms, seeds = 1:2)
  testplot <- plotSeeds(msd)
  vdiffr::expect_doppelganger("plotSeeds", testplot, path = "")
})

test_that("method 'plotTimes' plots correctly for multSimData objects", {
  skip_on_cran()
  msd <- getMultSimData(ms, times = 2*1:5)
  testplot <- plotTimes(msd, vars = "f", plottype = "violin")
  vdiffr::expect_doppelganger("plotTimes-violin", testplot, path = "")
  testplot <- plotTimes(msd, vars = "f", plottype = "dotplot")
  vdiffr::expect_doppelganger("plotTimes-dotplot", testplot, path = "")
})

#--------- tests for simplePdmp ---------

data("simplePdmp")
suppressMessages(
  ms <- multSim(simplePdmp, seeds = 1:10)
)

# test_that("method 'hist' plots correctly for multSim objects", {
#   expect_error(hist(ms, t = c(5, 10)))
#   testplot <- function() hist(ms, t = 10)
#   vdiffr::expect_doppelganger("hist-multSim", testplot, path = "")
# })

# test_that("method 'density' plots correctly for multSim objects", {
#   testplot <- function() invisible(density(ms, t = c(5, 10)))
#   vdiffr::expect_doppelganger("density-multSim", testplot, path = "")
# })

test_that("method 'plotTimes' plots correctly with a given threshold", {
  skip_on_cran()
  msd <- getMultSimData(ms, times = 2*1:5)
  testplot <- plotTimes(msd, vars = "f", plottype = "boxplot", threshold = 1)
  vdiffr::expect_doppelganger("plotTimes-boxplot", testplot, path = "")
})