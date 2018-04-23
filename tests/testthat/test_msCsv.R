context("multSimCsv")

#--------------- set working directory and simulate --------------

setwd(system.file("testdata", package = "pdmpsim", mustWork = TRUE))
unlink('mscsv*') # removes all files from working directory
data("simplePdmp")
suppressMessages({
  msCsv <- multSimCsv(simplePdmp, seeds = 1:3, prefix = "mscsv")
  ms <- multSim(simplePdmp, seeds = 1:3)
})

#-------------- tests ----------------

test_that("msCsv is of class 'multSimCsv' and most elements are correct", {
  expect_s3_class(msCsv, "multSimCsv")
  expect_equal(msCsv$seeds, 1:3)
  expect_equal(msCsv$model, simplePdmp)
  expect_type(msCsv$lafList, "list")
  expect_type(msCsv$timeList, "list")
})

test_that("element datamodel is correct", {
  dm1 <- LaF::detect_dm_csv("mscsv_Simulations_f.csv", header = TRUE)
  dm2 <- msCsv$datamodel
  expect_identical(dm1$dec, dm2$dec)
  expect_identical(dm1$sep, dm2$sep)
  expect_equal(dm1$columns, dm2$columns, check.attributes = FALSE)
  expect_identical(dm1$skip, dm2$skip)
  expect_identical(dm1$type, dm2$type)
})

test_that("filenames are correct", {
  allFiles <- list.files(path = ".")
  filenames <- sort(c("mscsv_MultSimCsv.rda", "mscsv_Simulations_d.csv", 
                      "mscsv_Simulations_f.csv"))
  expect_true(all(filenames %in% allFiles))
  expect_equal(filenames[2:3], sort(msCsv$csvList))
})

test_that("results ms and msCsv are equal", {
  expect_identical(msCsv$model, ms$model)
  expect_identical(msCsv$seeds, ms$seeds)
  expect_identical(names(msCsv$timeList), names(ms$timeList))
  expect_identical(length(msCsv$timeList), length(ms$timeList))
})

test_that("LaF links are correct", { #! hier wird im Quellcode die gleiche Funktion benutzt -> sinnloser test?
  loaded <- loadMultSimCsv("mscsv_MultSimCsv.rda")
  expect_equal(msCsv$seeds, unlist(loaded$lafList[[1]][,1], use.names = FALSE))
  expect_equal(msCsv$lafList[[2]][,20], loaded$lafList[[2]][,20])
})

test_that("appending seeds works as expected", {

  # seeds in .csv files at the moment: 1 2 3
  msCsv1 <- multSimCsv(simplePdmp, seeds = 2:4, prefix = "mscsv", append = TRUE)
  seedList <- data.frame(Seed = msCsv1$seeds)
  expect_equal(seedList, data.frame(Seed = 1:4))
  expect_equal(seedList, msCsv1$lafList[[1]][,1])

  # seeds in .csv files at the moment: 1 2 3 4
  msCsv2 <- multSimCsv(simplePdmp, seeds = 1:3, prefix = "mscsv", 
                       append = TRUE, uniqueSeeds = FALSE)
  seedList <- data.frame(Seed = msCsv2$seeds)
  expect_equal(seedList, data.frame(Seed = c(1:4, 1:3)))
  expect_equal(seedList, msCsv2$lafList[[1]][,1])

  # seeds in .csv files at the moment: 1 2 3 4 1 2 3
  expect_error(multSimCsv(simplePdmp, seeds = 2:5, prefix = "mscsv", 
                          append = TRUE, uniqueSeeds = TRUE))
  expect_warning(multSimCsv(simplePdmp, seeds = 2:5, prefix = "mscsv", 
                            append = TRUE, uniqueSeeds = FALSE))
  expect_equal(data.frame(Seed = c(1:4, 1:3, 2:5)), msCsv2$lafList[[1]][,1])
})
