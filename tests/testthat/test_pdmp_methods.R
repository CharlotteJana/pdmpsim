context("pdmp methods")

pdmp <- pdmpModel(
  descr = "a simple model",
  init = c(f = 10, d = 1),
  parms = list(α = 3),
  times = c(from = 0, to = 10, by = 0.01),
  discStates = list(d = c(-1, 1)),
  dynfunc = function(t, x, parms) c(x["d"]*t, 0),
  ratefunc = function(t, x, parms) 1,
  jumpfunc = function(t, x, parms, jtype) c(-x["f"], (-1)*x["d"])
)

#-------------- tests ----------------

test_that("printVect works as expected", {
  v1 <- c("a" = 1, "b" = 2, "c" = 3)
  v2 <- c("α" = 1/3, "∃" = 5000)
  expect_identical(printVect(v1), "a = 1, b = 2, c = 3")
  expect_identical(printVect(v2, sep = "~", collapse = "; "), "α~0.33; ∃~5000")
})

test_that("format works as expected", {
    expect_identical(format(pdmp), 
                   "Parms_α=3___Init_f=10_d=1___Times_0_10_0.01")
  expect_identical(format(pdmp, short=TRUE, slots="descr", end = " is tested"),
                   "a simple model is tested")
  expect_identical(format(pdmp, short=FALSE, slots = c("init", "parms")),
                   "Parameter: α = 3. Initial Values: f = 10, d = 1")
  expect_identical(format(pdmp, short=TRUE, begin = "model_", sep = "~"),
                   "model_Parms_α~3___Init_f~10_d~1___Times_0_10_0.01")
  expect_identical(format(pdmp, short=TRUE, collapse = "\n"),
                   "Parms_α=3\nInit_f=10_d=1\nTimes_0_10_0.01")
  expect_identical(format(pdmp, short = FALSE, slots = "discStates", end = "."),
                   "Discrete States: d ϵ {-1,1}.")
})

test_that("method 'print' prints something", {
  expect_output(print(pdmp), "An S4-object of class pdmpModel")
  expect_output(print(pdmp), "a simple model")
  expect_output(print(pdmp), "α: num 3")
  expect_output(print(pdmp), "Slot")
})
