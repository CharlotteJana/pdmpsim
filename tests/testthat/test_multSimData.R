context("multSimData")

ms <- readRDS(paste0(tempdir(), "/pdmpsimtest/test_MultSim.rda"))
msCsv <- loadMultSimCsv(paste0(tempdir(), "/pdmpsimtest/test_MultSimCsv.rda"))
sim <- getMultSimData(ms, seeds = c(1, 3))
simCsv <- getMultSimData(msCsv, seeds = c(1, 3))
slice <- getMultSimData(ms, times = c(1, 10))
sliceCsv <- getMultSimData(msCsv, times = c(1, 10))

test_that("methods for 'multSim' and 'multSimCsv' lead to identical results", {
  expect_equal(dplyr::arrange(sim, time, seed), 
               dplyr::arrange(simCsv, time, seed), tolerance = 1e-07)
  expect_equal(dplyr::arrange(slice, time, seed), 
               dplyr::arrange(sliceCsv, time, seed), tolerance = 1e-07)
})

test_that("non existing seeds are omitted", {
  expect_warning(
    sim2 <- getMultSimData(ms, seeds = c(1, 2000, 3))
    )
  expect_warning(
    simCsv2 <- getMultSimData(msCsv, seeds = c(1, 2000, 3))
    )
  expect_identical(sim, sim2)
  expect_identical(simCsv, simCsv2)
})

test_that("non existing time values are omitted", {
  expect_warning(
    slice2 <- getMultSimData(ms, times = c(1, -1, 10))
    )
  expect_warning(
    sliceCsv2 <- getMultSimData(msCsv, times = c(1, -1, 10))
    )
  expect_identical(slice, slice2)
  expect_identical(sliceCsv, sliceCsv2)
})