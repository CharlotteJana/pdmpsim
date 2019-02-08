### todo
# toggleSwitch: beide Varianten vergleichen
# insgesamt: den Code der wichtigen Modelle in einer Datei speichern

context("sim method simulates correctly")

test_that("sim is correct for a simple model", {

  simpleModel <- pdmpModel(
    descr = "a simple model",
    init = c(f = 10, d = 1),
    times = c(from = 0, to = 10, by = 0.01),
    discStates = list(d = c(-1, 1)),
    dynfunc = function(t, x, parms) c(x["d"]*t, 0),
    ratefunc = function(t, x, parms) 1,
    jumpfunc = function(t, x, parms, jtype) c(-x["f"], (-1)*x["d"])
  )

  times <- do.call(seq, as.list(times(simpleModel)))
  fpos <- function(x) 1/2 * x^2 + 10
  fneg <- function(x) -1/2 * x^2 - 10
  out <- sim(simpleModel, outSlot = FALSE, seed = 10)
  data <- cbind(times, d = out[,"d"], f = out[,"f"], 
                fpos = fpos(times), fneg = fneg(times))

  test <- NA
  for(i in seq_len(nrow(data))){
    test[i] <- switch(as.character(data[i,"d"]),
                      "1" = fpos(times[i]),
                      "-1" = fneg(times[i]),
                      NA)
  }

  expect_equal(data[,"f"], test, tolerance = 1e-05)
})

test_that("sim is correct for a pdmp with 2 jumptypes", {

  jtypePdmp <- pdmpModel(
    descr = "a pdmp with 2 jumptypes",
    init = c(f = 0, d = 0),
    times = c(from = 0, to = 10, by = 0.01),
    discStates = list(d = -1:1),
    dynfunc = function(t, x, parms) c(x["d"], 0),
    ratefunc = function(t, x, parms) c(1+x["d"], 1-x["d"]),
    jumpfunc = function(t, x, parms, jtype){
      c(0, switch(jtype, x["d"]-1, x["d"]+1))
    }
  )

  times <- do.call(seq, as.list(times(jtypePdmp)))
  fpos <- function(x) x
  fneg <- function(x) -x
  out <- sim(jtypePdmp, outSlot = FALSE, seed = 10)
  data <- cbind(times, d = out[,"d"], f = out[,"f"], dtest = NA, ftest = NA)
  dseq <- rle(data[, "d"])

  begin <- 1
  for(i in seq_along(dseq$values)){
    end <- begin + dseq$lengths[i] - 1
    data[begin:end, "ftest"] <- switch(as.character(dseq$values[i]),
                                       "-1" = fneg(times[0:dseq$length[i]]),
                                       "0" = 0,
                                       "1" = fpos(times[0:dseq$length[i]]),
                                       NA)
    begin <- end + 1
  }
  expect_equal(data[,"f"], data[,"ftest"], tolerance = 1e-02)
  #par(mfrow = c(1,2))
  #matplot(data[,"times"], data[,c("f", "ftest")], type = "l")
})

test_that("sim is correct for a pdmp with 2 discrete variables", {

  discPdmp <- pdmpModel(
    descr = "a pdmp with 2 discrete variables and 2 jumptypes",
    init = c(f = 0, d1 = 0, d2 = 0),
    times = c(from = 0, to = 10, by = 0.01),
    discStates = list(d1 = 0:1, d2 = 0:1),
    dynfunc = function(t, x, parms) c(x["d1"]-x["d2"], 0, 0),
    ratefunc = function(t, x, parms) c(1,1),
    jumpfunc = function(t, x, parms, jtype){
      c(0, switch(jtype, c((x["d1"]-1)^2, x["d2"]),
                  c(x["d1"], (x["d2"]-1)^2)))
    }
  )

  times <- do.call(seq, as.list(times(discPdmp)))
  fpos <- function(x) x
  fneg <- function(x) -x
  out <- sim(discPdmp, outSlot = FALSE, seed = 10)
  data <- cbind(times, d = out[,"d1"] - out[,"d2"], f = out[,"f"], 
                dtest = NA, ftest = NA)
  dseq <- rle(data[, "d"])

  begin <- 1
  for(i in seq_along(dseq$values)){
    end <- begin + dseq$lengths[i] - 1
    data[begin:end, "ftest"] <- switch(as.character(dseq$values[i]),
                                       "-1" = fneg(times[0:dseq$length[i]]),
                                       "0" = 0,
                                       "1" = fpos(times[0:dseq$length[i]]),
                                       NA)
    begin <- end + 1
  }
  expect_equal(data[,"f"], data[,"ftest"], tolerance = 1e-02)
  #matplot(data[,"times"], data[,c("f", "ftest")], type = "l")
})

test_that("variable 'initialize' works as expected", {
  skip("Slot 'initfunc' is not included yet.")
  initModel <- pdmpModel(
    init = c(f = 0, d = 1),
    initfunc = function(pdmp){
      init(pdmp) <- c(f = pdmp@init[[1]] + 1, d = 1)
      invisible(pdmp)
    },
    times = c(from = 0, to = 10, by = 0.01),
    dynfunc = function(t, x, parms) c(x["d"]*t, 0),
    ratefunc = function(t, x, parms) 1,
    jumpfunc = function(t, x, parms, jtype) c(-x["f"], (-1)*x["d"])
  )
  expect_equal(init(initModel)[1], 1, check.names = FALSE)
  initModel <- sim(initModel, initialize = TRUE)
  expect_equal(out(initModel)[1, 2], 2, check.names = FALSE)
})