context("generator")

test_that("method generator is correct for a pdmp with one discrete variable", {
  skip_if_not_installed("Deriv")
  
  simplePdmp <- pdmpModel(
    descr = "a simple pdmp",
    init = c(f = 0, d = 0),
    times = c(from = 0, to = 10, by = 0.01),
    discStates = list(d = c(-1, 0, 1)),
    dynfunc = function(t, x, parms) c(x["d"], 0),
    ratefunc = function(t, x, parms) c(1+x["d"], 1-x["d"]),
    jumpfunc = function(t, x, parms, jtype){
      c(0, switch(jtype, x["d"]-1, x["d"]+1))
    }
  )

  fvals <- seq(from = 0, to = 3, by = 0.5)

  g <- function(d, f) d*f
  Qg <- function(d, fvals) 
    sapply(fvals, function(f) 
      generator(simplePdmp)(g)(t = 10, x = c("d" = d, "f" = f)))
  test1 <- cbind(Qg(-1, fvals), 
                 Qg(0, fvals), 
                 Qg(1, fvals))
  test2 <- cbind(1+2*fvals, 
                 rep(0, length(fvals)), 
                 1-2*fvals)
  expect_equal(test1, test2)

  h <- function(d, f) exp(f) + d
  Qh <- function(d, fvals) 
    sapply(fvals, function(f) 
      generator(simplePdmp)(h)(t = 10, x = c("d" = d, "f" = f)))
  test1 <- cbind(Qh(-1, fvals), 
                 Qh(0, fvals), 
                 Qh(1, fvals))
  test2 <- cbind(-3*exp(fvals)+4, 
                 -2*exp(fvals)+2 , 
                 -exp(fvals))
  expect_equal(test1, test2)
})

test_that("method generator is correct for a pdmp with 2 discrete variables", {
  skip_if_not_installed("Deriv")
  
  discPdmp <- pdmpModel(
    descr = "a pdmp with 2 discrete variables and 2 jumptypes",
    init = c(f = 0, d1 = 0, d2 = 0),
    discStates = list(d1 = 0:1, d2 = 0:1),
    times = c(from = 0, to = 10, by = 0.01),
    dynfunc = function(t, x, parms) c(x["d1"]-x["d2"], 0, 0),
    ratefunc = function(t, x, parms) c(1,1),
    jumpfunc = function(t, x, parms, jtype){
      c(0, switch(jtype, c((x["d1"]-1)^2, x["d2"]),
                  c(x["d1"], (x["d2"]-1)^2)))
    }
  )

  # The simulations of this model are equivalent ot the simulations of the 
  # model used in the test before. Therefore the generators should be the same.

  fvals <- seq(from = 0, to = 3, by = 0.5)

  g <- function(d1, d2, f) (d1-d2)*f
  Qg <- function(d1, d2, fvals) 
    sapply(fvals, function(f)
      generator(discPdmp)(g)(t = 10, x = c("d1" = d1, "d2" = d2, "f" = f)))
  test1 <- cbind(Qg(0, 1, fvals), 
                 Qg(0, 0, fvals), 
                 Qg(1, 1, fvals), 
                 Qg(1, 0, fvals))
  test2 <- cbind(1+2*fvals, 
                 rep(0, length(fvals)),  
                 rep(0, length(fvals)), 
                 1-2*fvals)
  expect_equal(test1, test2)

  h <- function(d1, d2, f) exp(f) + d1 - d2
  Qh <- function(d1, d2, fvals) 
    sapply(fvals, function(f) 
      generator(discPdmp)(h)(t = 10, x = c("d1" = d1, "d2" = d2, "f" = f)))
  test1 <- cbind(Qh(0, 1, fvals), 
                 Qh(0, 0, fvals), 
                 Qh(1, 1, fvals), 
                 Qh(1, 0, fvals))
  test2 <- cbind(-3*exp(fvals)+4, 
                 -2*exp(fvals)+2 , 
                 -2*exp(fvals)+2, 
                 -exp(fvals))
  expect_equal(test1, test2)
})

test_that("method generator is correct for the toggle switch model", {
  skip_if_not_installed("Deriv")
  
  toggleSwitch <- pdmpModel(
      descr = "toggle switch with two promotors",
      parms = list(bA = 0.5, bB = 1,   aA = 2, aB = 4, 
                   k01A = 0.5, k10A = 2, k01B = 1/3, k10B = 3),
      init = c(fA = 0.5, fB = 0.5, dA = 1, dB = 1),
      discStates = list(dA = 0:1, dB = 0:1),
      dynfunc = function(t, x, parms) {
        df <- with(as.list(c(x, parms)),
                   c(-bA*fA + aA*dA, -bB*fB + aB*dB))
        return(c(df, 0, 0))
      },
      ratefunc = function(t, x, parms) {
        return(with(as.list(c(x, parms)),
                    c(switch(dB+1, k01A, k10A*fB),
                      switch(dA+1, k01B, k10B*fA))))
      },
      jumpfunc = function(t, x, parms, jtype) {
        return(with(as.list(c(x, parms)),
                    c(fA, fB, switch(jtype, c(dA, 1-dB), c(1-dA, dB)))))
      },
      times = c(from = 0, to = 10, by = 0.01)
      )

  fvals <- expand.grid(1:10, -1:3)
  fAs <- fvals[,1]
  fBs <- fvals[,2]

  g <- function(dA, dB, fA, fB) fA * fB + dA # the order of the arguments 
                                             # is different to the order in 
                                             # init(toggleSwitch)
  Qg <- function(dA, dB, fvals) {
    sapply(seq_len(nrow(fvals)), function(i) 
      generator(toggleSwitch)(g)(t = 10, x = c("dA" = dA, "dB" = dB, 
                                               "fA" = fvals[i,1], 
                                               "fB" = fvals[i,2])))
    }
  test1 <- cbind(Qg(0, 0, fvals), 
                 Qg(0, 1, fvals), 
                 Qg(1, 0, fvals), 
                 Qg(1, 1, fvals))
  test2 <- with(as.list(parms(toggleSwitch)), -bA*fAs*fBs - bB*fBs*fAs +
                  cbind(k01B, k01B + aB*fAs, aA*fBs -k10B*fAs, 
                        aA*fBs - k10B*fAs + aB*fAs))
  dimnames(test2) <- NULL
  expect_equal(test1, test2)
})
