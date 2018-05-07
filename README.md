
<!-- README.md is generated from README.Rmd. Please edit that file -->
pdmpsim
=======

The goal of pdmpsim is to simulate piecewise deterministic markov processes (PDMPs) within R and to provide methods for analysing the simulation results.

It is possible to

-   simulate pdmps
-   store multiple simulations in a convenient way
-   calculate some statistics on them
-   plot the results (there are different plot methods available)
-   compute the generator

The PDMPs can have multiple discrete and continous variables, but are not allowed to have borders or a varying number of continous variables (the number should be independent of the state of the discrete variable).

Installation
------------

You can install pdmpsim from github with:

``` r
# install.packages("devtools")
devtools::install_github("CharlotteJana/pdmpsim")
```

Example
-------

This is a simple example modelling gene expression with positive feedback:

``` r
examplePDMP <- pdmpModel(
  descr = "Gene regulation with positive feedback",
  parms = list(β = 0.2, α = 7, κ10 = 0.04, κ01 = 0.02),
  init = c(f = 1, d = 1),
  discStates = list(d = c(0, 1)),
  dynfunc = function(t, x, parms) {
    df <- with(as.list(c(x, parms)), α*d - β*f)
    return(c(df, 0))
  },
  ratefunc = function(t, x, parms) {
    return(with(as.list(c(x, parms)), switch(d + 1, κ01*f, κ10)))
  },
  jumpfunc = function(t, x, parms, jtype) {
    c(x[1], 1 - x[2])
  },
  times = c(from = 0, to = 100, by = 0.1),
  solver = "lsodar")
```

Perform one simulation and plot the result:

``` r
examplePDMP <- sim(examplePDMP)
plot(examplePDMP)
```

Simulate multiple times and store the result:

``` r
simulations <- multSim(examplePDMP, seeds = 1:30)
```

Plot the simulations:

``` r
plot(simulations)
plotTimes(simulations, vars = "f", times = c(60, 90, 100))
density(simulations, t = c(60, 90, 100))
plotSeeds(simulations, seeds = c(1, 30))
```

Calculate minimum, mean and maximum and plot them:

``` r
summarise_at(simulations, .vars = "f", .funs = c("min", "mean", "max"))
plotStats(simulations, vars = "f", funs = c("min", "mean", "max"))
```
