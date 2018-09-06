context("accessors")

data("simplePdmp")
obj <- simplePdmp

test_that("changing of important slots resets slot out", {

    # slot init
    obj <- sim(obj)
    init(obj) <- c(f = 5, d = 1)
    expect_null(out(obj))

    # slot parms
    obj <- sim(obj)
    parms(obj) <- c(a = 3)
    expect_null(out(obj))

    # slot times
    obj <- sim(obj)
    times(obj) <- c(from = 0, to = 10, by = 0.1)
    expect_null(out(obj))

    # slot dynfunc
    obj <- sim(obj)
    dynfunc(obj) <- function(t, x, parms) c(-x["d"]*t, 0)
    expect_null(out(obj))

    # slot ratefunc
    obj <- sim(obj)
    ratefunc(obj) <- function(t, x, parms) 2
    expect_null(out(obj))

    # slot jumpfuc
    obj <- sim(obj)
    jumpfunc(obj) <- function(t, x, parms, jtype) c(0, (-1)*x["d"])
    expect_null(out(obj))

    # slot solver
    obj <- sim(obj)
    solver(obj) <- "lsodar"
    expect_null(out(obj))

    # slot initfunc
    obj <- sim(obj, initialize = FALSE)
    initfunc(obj) <- NULL
    expect_null(out(obj))
    
    # slot discStates
    obj <- sim(obj, initialize = FALSE)
    discStates(obj) <- list(d = c(1, 0, -1))
    expect_null(out(obj))

})

test_that("changing of slots descr does not affect slot out", {

  # slot descr
  obj <- sim(obj)
  descr(obj) <- "another description"
  expect_false(is.null(out(obj)))

})

test_that("unused slots inherited by 'simObj' can not be set",{

  # slot main
  expect_warning(
    main(obj) <- function(time, init, parms) list(c(0, 0))
    )

  # slot equations
  expect_warning(
    equations(obj) <- list(function(time, init) time*init[1])
    )

  # slot observer
  expect_warning(
    observer(obj) <- function(time, init) time*init[1]
  )

})
