context("getTimeslice")

setwd(system.file("testdata", package = "pdmpsim", mustWork = TRUE))
ms <- readRDS("test_MultSim.rda")
msCsv <- loadMultSimCsv("test_MultSimCsv.rda")
slice <- getTimeslice(ms, times = c(1, 10))
sliceCsv <- getTimeslice(msCsv, times = c(1, 10))

test_that("methods for 'multSim' and 'multSimCsv' lead to identical results", {
  expect_equal(slice, sliceCsv, tolerance = 1e-07)
})

test_that("non existing time values are omitted", {
  expect_warning(slice2 <- getTimeslice(ms, times = c(1, 1000, 10)))
  expect_warning(sliceCsv2 <- getTimeslice(msCsv, times = c(1, 1000, 10)))
  expect_identical(slice, slice2)
  expect_identical(sliceCsv, sliceCsv2)
})