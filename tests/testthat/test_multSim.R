context("multSim")

data("toggleSwitch")
model <- toggleSwitch
times(model) <- c(from = 0, to = 5, by = 0.1)

out1 <- sim(model, outSlot = FALSE, seed = 1)
out2 <- sim(model, outSlot = FALSE, seed = 2)

test_that("multsim stores simulation results correctly", {
  ms <- multSim(model, seeds = c(2,1))
  expect_identical(ms$outputList[[1]], out2)
  expect_identical(ms$outputList[[2]], out1)
  expect_equal(ms$seeds, 2:1)
})

test_that("multSim expands seeds", {
  ms <- multSim(model, seeds = 1)
  msg <- capture_messages(ms <- multSim(model, seeds = 2:1, ms = ms))
  expect_true(all(!grepl("1", msg))) # message should not contain substring "1"
  expect_identical(ms$outputList[[1]], out2)
  expect_identical(ms$outputList[[2]], out1)
  
  expect_message(ms2 <- multSim(model, seeds = 1:2, ms = ms))
  expect_identical(ms$outputList[[1]], ms2$outputList[[2]])
})

test_that("multSim deletes seeds iff allowDeletion = TRUE", {
  ms <- multSim(model, seeds = c(1, 3))
  msShort <- multSim(model, seeds = 2:4, ms = ms, allowDeletion = TRUE)
  msLong <- multSim(model, seeds = 2:4, ms = ms, allowDeletion = FALSE)
  expect_equal(length(msShort$outputList), 3)
  expect_equal(length(msLong$outputList), 4)
})

test_that("multSim expands times", {
  ms <- multSim(model, seeds = 1)
  times(model)["to"] <- times(model)["to"]+1
  ms <- multSim(model, seeds = 1:2, ms = ms)
  expect_equal(ms$outputList[[2]][, 1], fromtoby(times(model)))
})

test_that("multSim handles wrong input arguments", {
  expect_error(multSim(model, seeds = 1, filename = "testfile"))
  
  ms <- multSim(model, seeds = NULL)
  expect_equal(0, length(ms$outputList))
  
  changedModel <- model
  init(changedModel) <- init(model) + 1
  expect_error(multSim(changedModel, seeds = 1, ms = ms))
  
  changedModel <- model
  descr(changedModel) <- "new description"
  expect_error(multSim(changedModel, seeds = 1, ms = ms))
})