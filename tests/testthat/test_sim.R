context("sim method")

data("simplePdmp")
obj <- simplePdmp

test_that("the results of 2 simulations with the same seed are equal", {

  out1 <- sim(obj, outSlot = FALSE, seed = 5)
  out2 <- sim(obj, outSlot = FALSE, seed = 5)
  expect_identical(out1, out2)
})

test_that("the results of 2 simulations with different seeds are different", {

  out1 <- sim(obj, outSlot = FALSE, seed = 5)
  out2 <- sim(obj, outSlot = FALSE, seed = 3)
  expect_false(identical(out1, out2))
})

test_that("parameter outSlot behaves correctly", {

  obj <- sim(obj)
  expect_s4_class(obj, "pdmpModel")
  expect_false(is.null(out(obj)))

  out <- sim(obj, outSlot = FALSE)
  expect_s3_class(out, "deSolve")
})

test_that("if initialize = TRUE, slot 'initfunc' is called before sim", {
  skip("Slot 'initfunc' is not included yet.")

  model1 <- sim(obj, seed = 1)
  obj@initfunc <- function(obj){
    init(obj) <- c("f" = runif(1, max = 20), "d" = 1)
    invisible(obj)
  }
  model2 <- sim(obj, seed = 1, initialize = TRUE)
  model3 <- sim(obj, seed = 1, initialize = FALSE)

  expect_false(identical(out(model1), out(model2)))
  expect_false(identical(init(model1), init(model2)))
  expect_identical(out(model1), out(model3))
})

test_that("outrate = TRUE only affects column 'pdmpsim:negcumrate'", {
  out1 <- sim(obj, seed = 1, outrate = TRUE, outSlot = FALSE)
  out1 <- out1[, -which(colnames(out1) =="pdmpsim:negcumrate")]
  
  out2 <- sim(obj, seed = 1, outrate = FALSE, outSlot = FALSE)
  class(out2) <- "matrix"
  
  expect_identical(out1, out2)
})

test_that("order of variables in init doesn't matter for sim", {
  data(toggleSwitch)
  times(toggleSwitch) <- c(from = 0, to = 10, by = 0.1)
  init(toggleSwitch) <-  c(fA = 0.5, fB = 0.5, dA = 1.0, dB = 1.0)
  sim1 <- sim(toggleSwitch, seed = 3, outSlot = FALSE)
  
  model <- pdmpModel(
    descr = "Toggle Switch different order of variables",
    parms = parms(toggleSwitch),
    init = c(fA = 0.5, dA = 1.0, fB = 0.5, dB = 1.0),
    discStates = list(dA = c(0, 1), dB = c(0, 1)),
    times = times(toggleSwitch),
    dynfunc = function(t, x, parms) {
      df <- with(as.list(c(x, parms)), c(-bA*fA + aA*dA, -bB*fB + aB*dB))
      return(c(df[1], 0, df[2], 0))
    },
    ratefunc = function(t, x, parms) {
      return(with(as.list(c(x, parms)), c(switch(dB+1, k01B, k10B*fA),
                                          switch(dA+1, k01A, k10A*fB))))
    },
    jumpfunc = function(t, x, parms, jtype){
      return(with(as.list(c(x, parms)), c(fA, switch(jtype, dA, 1-dA), 
                                          fB, switch(jtype, 1-dB, dB))))
    })
  sim2 <- sim(toggleSwitch, seed = 3, outSlot = FALSE)
  
  expect_identical(sim1, sim2)
})