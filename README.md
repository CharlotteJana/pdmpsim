Package pdmpsim
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

The goal of pdmpsim is to simulate [piecewise deterministic markov
processes](https://www.researchgate.net/publication/316281383_Piecewise-deterministic_Markov_processes_A_general_class_of_non-diffusion_stochastic_models_and_Discussion)
(PDMPs) within R and to provide methods for analysing the simulation
results.

It is possible to

  - simulate PDMPs
  - store multiple simulations in a convenient way
  - calculate some statistics on them
  - plot the results (there are different plot methods available)
  - compute the generator numerically

The PDMPs can have multiple discrete and continous variables. They are
not allowed to have boundaries or a varying number of continous
variables (the number should be independent of the state of the discrete
variable).

You can install `pdmpsim` from github with:

``` r
# install.packages("devtools")
devtools::install_github("CharlotteJana/pdmpsim")
```

# A simple example

This is a simple example modelling gene expression with positive
feedback:

``` r
examplePDMP <- pdmpModel(
  descr = "Gene regulation with positive feedback",
  parms = list(b = 0.2, a = 7, k10 = 0.04, k01 = 0.02),
  init = c(f = 1, d = 1),
  discStates = list(d = c(0, 1)),
  dynfunc = function(t, x, parms) {
    df <- with(as.list(c(x, parms)), a*d - b*f)
    return(c(df, 0))
  },
  ratefunc = function(t, x, parms) {
    return(with(as.list(c(x, parms)), switch(d + 1, k01*f, k10)))
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

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

# Multiple simulations

Package `pdmpsim` provides two similar methods to perform and store a
large number of different simulations of one PDMP.

Function `multSim` returns an S3-object of class `multSim` which
contains a list of simulation results, a list of time values declaring
the time needed for the corresponding simulation, the model that was
used for simulation and a vector of numeric numbers. This vector is
named `seeds`, its elements are used as argument to function `sim` and
control the stochastic part of the model, making the simulation results
reproducable. The vector `seeds` and the PDMP model are the only
arguments needed for function `multSim`.

``` r
simulations <- multSim(examplePDMP, seeds = 1:10)
#> Calculating seeds  1  2  3  4  5  6  7  8  9  10
```

The second function available to store multiple simulations is called
`multSimCsv`. It is only useful if the memory used by all simulations
exceeds the working memory. In this case, it is not possible to store
all simulations in one object. They are stored in multiple csv files
instead.

# Plot methods

There are several functions available to plot the simulations stored as
`multSim`.

## Plot single simulations

``` r
plotSeeds(simulations, seeds = c(1, 30))
#> Warning in getMultSimData.multSim(x, seeds = seeds): There are no
#> simulations for seed 30
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

## Heatmap over all simulations

``` r
plot(simulations)
#> `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

## Boxplot and violin plot

``` r
plotTimes(simulations, vars = "f", times = c(60, 90, 100), nolo = 3)
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

## Density plot and histogram

``` r
density(simulations, t = c(60, 90, 100))
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

## Calculate and plot statistics

Calculate minimum, mean and maximum and plot them:

``` r
summarise_at(simulations, .vars = "f", .funs = c("min", "mean", "max"))
#> # A tibble: 1,001 x 4
#>     time   min  mean   max
#>    <dbl> <dbl> <dbl> <dbl>
#>  1   0    1     1     1   
#>  2   0.1  1.67  1.67  1.67
#>  3   0.2  2.33  2.33  2.33
#>  4   0.3  2.98  2.98  2.98
#>  5   0.4  3.43  3.60  3.61
#>  6   0.5  3.36  4.15  4.24
#>  7   0.6  3.30  4.69  4.84
#>  8   0.7  3.23  5.22  5.44
#>  9   0.8  3.17  5.74  6.03
#> 10   0.9  3.11  6.25  6.60
#> # … with 991 more rows
plotStats(simulations, vars = "f", funs = c("min", "mean", "max"))
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

# A more sophisticated example

The PDMP presented in this paragraph models a gene regulation mechanism
with toggle switch between two genes. We consider two genes \(A\) and
\(B\), and the concentration of their gene products, \(f_A\) and
\(f_B\). A product of gene \(A\) can block the transcription of gene
\(B\) and therefore affects the concentration \(f_B\). Conversely, a
product of gene \(B\) can block gene \(A\).

\[\]
(<https://raw.githubusercontent.com/CharlotteJana/pdmppoly/charlotte/man/figures/README-toggleSwitch.png>)

The PDMPs contains six different constant parameters:
\(k_{01}, k_{10}, a_A, a_B, b_A\) and \(b_B\). The formulation as
`pdmpModel` object ist as follows:

``` r

toggleSwitch <- pdmpModel(
    descr = "Toggle switch with two promotors",
    parms = list(bA = 0.5, bB = 0.5, aA = 2, aB = 4, k01 = 0.5, k10 = 2),
    init = c(fA = 0.5, fB = 0.5, dA = 1.0, dB = 1.0),
    discStates = list(dA = c(0, 1), dB = c(0, 1)),
    times = c(from = 0, to = 100, by = 0.01),
    dynfunc = function(t, x, parms) {
       df <- with(as.list(c(x, parms)), c(-bA*fA + aA*dA, -bB*fB + aB*dB))
       return(c(df, 0, 0))
    },
    ratefunc = function(t, x, parms) {
       return(with(as.list(c(x, parms)), c(switch(dB+1, k01, k10*fA),
                                           switch(dA+1, k01, k10*fB))))
    },
    jumpfunc = function(t, x, parms, jtype){
       return(with(as.list(c(x, parms)), c(fA, fB, switch(jtype,
                                                          c(dA, 1-dB),
                                                          c(1-dA, dB)))))
    })
```

The model is included in package `pdmpsim` as an example, a detailed
description of the different slots can be loaded with `?toggleSwitch`.

``` r
plot(sim(toggleSwitch), ggplot = TRUE) + 
  ggplot2::scale_color_manual(values = c("orange", "palegreen3")) # change color
```

![](man/figures/README-unnamed-chunk-11-1.png)<!-- -->

# License

Some classes introduced in package `pdmpsim` are based on code from
package [simecol](http://simecol.r-forge.r-project.org/). Both packages
are free open source software licensed under the [GNU Public
License](https://www.gnu.org/licenses/#GPL) (GPL 2.0 or above). The
software is provided as is and comes WITHOUT WARRANTY.
