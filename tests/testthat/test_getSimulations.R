context("getSimulations")

setwd(system.file("testdata", package = "pdmpsim", mustWork = TRUE))
ms <- readRDS("test_MultSim.rda")
msCsv <- loadMultSimCsv("test_MultSimCsv.rda")
sim <- getSimulations(ms, seeds = c(1, 3))
simCsv <- getSimulations(msCsv, seeds = c(1, 3))

test_that("methods for 'multSim' and 'multSimCsv' lead to identical results", {
  expect_equal(sim, simCsv, tolerance = 1e-07)
})

test_that("non existing seeds are omitted", {
  expect_warning(sim2 <- getSimulations(ms, seeds = c(1, 2000, 3)))
  expect_warning(simCsv2 <- getSimulations(msCsv, seeds = c(1, 2000, 3)))
  expect_identical(sim, sim2)
  expect_identical(simCsv, simCsv2)
})