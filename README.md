
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmrefpoints: Projecting long-term marine mammal abundance with bycatch

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://github.com/mcsiple/mmrefpoints/issues)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6336011.svg)](https://doi.org/10.5281/zenodo.6336011)

<!-- badges: end -->

`mmrefpoints` is an R package that generates marine mammal population
projections based on starting abundance, life history, and bycatch
rates, based on the BALEEN II population dynamics model.

## Authors

Margaret C. Siple  
André E. Punt  
Tessa B. Francis  
Philip S. Hammond  
Dennis Heinemann  
Kristy J. Long  
Jeffrey E. Moore  
Maritza Sepúlveda  
Randall R. Reeves  
Guðjón Már Sigurðsson  
Gísli Víkingsson  
Paul R. Wade  
Rob Williams  
Alexandre N. Zerbini

## Contents

-   [Need](#need)
-   [Details](#details)
-   [Installation](#installation)
-   [Contributing](#contributing)
-   [References](#references) <!-- end toc -->

## Need

Stakeholders involved in the management of marine mammal bycatch in
marine fisheries need tools to simulate the effects of management
decisions on marine mammal populations. Population models are a key part
of this process. This package contains the tools to simulate marine
mammal populations and an app that shows model outputs in a
user-friendly way.

## Details

This R package contains the functions used in the Marine Mammal Bycatch
Impacts Exploration Tool (MMBIET), a Shiny app built by Margaret Siple,
André Punt, and the Ocean Modeling Forum’s [Marine Mammal Bycatch
Working
Group](https://oceanmodelingforum.org/working-groups/marine-mammal-bycatch-working-group/).

The functions in this package, and the Shiny app, are intended to be
used in cases where data on bycatch and/or population status are sparse
or unavailable.

Our target audience is stakeholders interested in projecting marine
mammal populations to examine the impacts of bycatch. Those code could
also be used as a teaching tool, or for anyone who is more familiar with
R than FORTRAN and wants to use some components of the BALEEN II model
(Punt 1999).

## Installation

This package can be downloaded directly from GitHub:

    devtools::install_github("mcsiple/mmrefpoints")

-   NOTE: For Linux users, if you run into an error about the `magick`
    package, try installing `magick` first using the instructions
    [here](https://cran.r-project.org/web/packages/magick/vignettes/intro.html).

## Contributing [![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/dwyl/esta/issues)

### Community guidelines

We would like this package to be sustainable in the long term and
welcome contributions. If you are interested in contributing, please
check out our [Contribution
Guide](https://github.com/mcsiple/mmrefpoints/blob/master/CONTRIBUTING.md).

Bugs and enhancements are tracked through GitHub issues. If you have a
bug to report, there is a [bug report
template](https://github.com/mcsiple/mmrefpoints/blob/master/.github/ISSUE_TEMPLATE/bug_report.md)
to help maximize the benefit of your report for everyone. The same goes
for requests for
[enhancements](https://github.com/mcsiple/mmrefpoints/blob/master/.github/ISSUE_TEMPLATE/feature_request.md).

## Accessing the MMBIET Shiny app

The functions in this package can also be accessed through the Shiny app
for this project, which is located online
[here](https://msiple.shinyapps.io/mmrefpoints/). The app provides an
easy way to explore outcomes and print out a report with inputs and
outputs.

The mmBIET Shiny app can also be accessed through the R package:

``` r
library(mmrefpoints)
run_app()
```

<img src="https://github.com/mcsiple/ltbycatch/blob/master/docs/screenshot1.png" alt="screenshot1" width="400">

## Functionality

The foundation of this package is an age-structured population
projection model with bycatch mortality. Key functions in this package:

| Function      | Purpose                                                         |
|:--------------|:----------------------------------------------------------------|
| dynamics()    | Generate a single trajectory for marine mammal population size  |
| projections() | Generate several trajectories for marine mammal population size |

To create a single projection for a marine mammal population, use the
`dynamics()` function:

``` r
x <- mmrefpoints::dynamics(lh.params = list(S0 = 0.944, S1plus = 0.99, 
                           K1plus = 9000, AgeMat = 17, z = 2.39, nages = 25, lambdaMax = 1.04),
                           InitDepl = 0.6, 
                           ConstantCatch = NA, 
                           ConstantF = rep(0.01, times = 100), 
                           nyears = 100)
plot(1:100, x$TotalPop, type = 'l', xlab = "Year", ylab = "Population size")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

Variation in the model is introduced through variation in bycatch
mortality over time and uncertainty in the estimate of starting
abundance.

``` r
x <- mmrefpoints::projections(
  NOut = 1,
  ConstantBycatch = list(Catch = 50, CV = 0.7),
  InitDepl = 0.6,
  lh.params = list(
    S0 = 0.944, S1plus = 0.99,
    K1plus = 9000, AgeMat = 18, nages = 25, z = 2.39, lambdaMax = 1.04
  ),
  nyears = 100, obs_CV = 0.1
)

# One trajectory with bycatch uncertainty and an observation CV:
plot(x$trajectories, type = "l", xlab = "Year", ylab = "Population size")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Projections shown in the app are based on simulation parameters provided
by the user. They include a “high”, “medium”, and “low” bycatch level
based on a user-determined range.

``` r
x_lo <- mmrefpoints::projections(
  NOut = 100,
  ConstantBycatch = list(Catch = 0, CV = 0),
  InitDepl = 0.6,
  lh.params = list(
    S0 = 0.944, S1plus = 0.99,
    K1plus = 9000, AgeMat = 18, nages = 25, z = 2.39, lambdaMax = 1.04
  ),
  nyears = 100, obs_CV = 0.1
)

x_med <- mmrefpoints::projections(
  NOut = 100,
  ConstantBycatch = list(Catch = 50, CV = 0.7),
  InitDepl = 0.6,
  lh.params = list(
    S0 = 0.944, S1plus = 0.99,
    K1plus = 9000, AgeMat = 18, nages = 25, z = 2.39, lambdaMax = 1.04
  ),
  nyears = 100, obs_CV = 0.1
)

x_hi <- mmrefpoints::projections(
  NOut = 100,
  ConstantBycatch = list(Catch = 200, CV = 0.7),
  InitDepl = 0.6,
  lh.params = list(
    S0 = 0.944, S1plus = 0.99,
    K1plus = 9000, AgeMat = 18, nages = 25, z = 2.39, lambdaMax = 1.04
  ),
  nyears = 100, obs_CV = 0.1
)

mmrefpoints::plot_proj(high = x_hi,
                       med = x_med,
                       low = x_lo,
                       years.plot = 100,
                       ylims = c(0, 9000),
                       K1plus = 9000)
#> Warning: Removed 38 row(s) containing missing values (geom_path).
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

## References

Punt, A. E. 1999. Annex R: A full description of the standard Baleen II
model and some variants thereof. Division of Marine Research, CSIRO
Marine Laboratories, Hobart, Australia. Available from
<https://duwamish.lib.washington.edu/uwnetid/illiad.dll?Action=10&Form=75&Value=1651729>
(accessed August 7, 2018).

## How to cite

To cite this package or the MMBIET Shiny app, please use the following
citation:

> Margaret C. Siple, André E. Punt, Tessa B. Francis, Philip S. Hammond,
> Dennis Heinemann, Kristy J. Long, Jeffrey E. Moore, Randall R. Reeves,
> Sepúlveda Maritza, Guðjón Már Sigurðsson, Gísli Víkingsson, Paul R.
> Wade, Rob Williams and Alexandre N. Zerbini (NA). mmrefpoints: Project
> Marine Mammal Populations and Calculate Reference Points. R package
> version 1.0.1. url: <https://github.com/mcsiple/mmrefpoints> doi:
> 10.5281/zenodo.4758401

NOTE that if you want to cite all versions of the software, you can use
the doi
[10.5281/zenodo.4758401](https://zenodo.org/record/5949332#.Yf1infXMI-Q).
When additional releases happen, there will be a doi for each new
release as well.
