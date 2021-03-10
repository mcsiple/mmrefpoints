
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmrefpoints: Projecting long-term marine mammal abundance with bycatch

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`mmrefpoints` is an R package that generates marine mammal population
projections based on starting abundance, life history, and bycatch
rates, based on the BALEEN II population dynamics model.

## Authors

Margaret C. Siple  
André E. Punt  
Tessa B. Francis  
Phil S. Hammond  
Dennis Heinemann  
Kristy J. Long  
Jeffrey E. Moore  
Maritza Sepulveda  
Randall R. Reeves  
Guðjón Már Sigurðsson  
Gísli Víkingsson  
Paul R. Wade  
Rob Williams  
Alexandre N. Zerbini

## Contents

  - [Details](#details)
  - [Install](#install)
  - [Contributing](#contributing)
  - [References](#references) <!-- end toc -->

## Details

This R package contains the functions used in the Marine Mammal Bycatch
Impacts Exploration Tool (mmBIET), a Shiny app built by Margaret Siple,
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

## Install

This package can be downloaded directly from GitHub:

    devtools::install_github("mcsiple/mmrefpoints")

## Contributing [![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)](https://github.com/dwyl/esta/issues)

We would like this package to be sustainable in the long term and
welcome contributions. If you encounter a bug, please leave a note on
the Issues page. You can also leave comments there about additional
functionality (please add the “enhancement” label). If you are
interested in contributing, we direct you to the R package [contribution
advice](http://r-pkgs.had.co.nz/git.html) from Hadley Wickham.

## Accessing the mmBIET Shiny app

The functions in this package can also be accessed through the Shiny app
for this project, which is located online
[here](https://msiple.shinyapps.io/mammaltool/). The app provides an
easy way to explore outcomes and print out a report with inputs and
outputs.

The mmBIET Shiny app can also be accessed through the R package:

    #> Loading required package: tidyverse
    #> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
    #> ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    #> ✓ tibble  3.1.0     ✓ dplyr   1.0.5
    #> ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    #> ✓ readr   1.4.0     ✓ forcats 0.5.1
    #> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    #> x dplyr::filter() masks stats::filter()
    #> x dplyr::lag()    masks stats::lag()
    #> 
    #> Attaching package: 'mmrefpoints'
    #> The following object is masked from 'package:datasets':
    #> 
    #>     lh
    #> Loading required package: shiny
    #> 
    #> Listening on http://127.0.0.1:7165
    #> [1] "Language change: en"

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

<img src="https://github.com/mcsiple/ltbycatch/blob/master/docs/screenshot1.png" alt="screenshot1" width="400">

## References

Punt, A. E. 1999. Annex R: A full description of the standard Baleen II
model and some variants thereof. Division of Marine Research, CSIRO
Marine Laboratories, Hobart, Australia. Available from
<https://duwamish.lib.washington.edu/uwnetid/illiad.dll?Action=10&Form=75&Value=1651729>
(accessed August 7, 2018).

## Citation (t.b.d.)

Margaret C. Siple, André E. Punt, Tessa B. Francis, Phil S. Hammond,
Dennis Heinemann, Kristy J. Long, Jeff Moore, Maritza Sepulveda, Randall
R. Reeves, Guðjón Már Sigurðsson, Gísli Víkingsson, Paul R. Wade, Rob
Williams, and Alexandre N. Zerbini (t.b.d.). mmrefpoints: Projecting
long-term marine mammal abundance with bycatch. R package version 1.0.0.
url: <https://github.com/mcsiple/ltbycatch>
