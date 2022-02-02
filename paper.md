---
title: 'mmrefpoints: Projecting long-term marine mammal abundance with bycatch'
tags:
  - Shiny
  - R
  - marine mammal bycatch
authors:
  - name: Margaret C. Siple^[*]
    orcid: 0000-0002-4260-9856
    affiliation: 1
  - name: André E. Punt
    orcid: 0000-0001-8489-2488
    affiliation: 2
  - name: Tessa B. Francis
    orcid: 0000-0002-3383-5392
    affiliation: 3
  - name: Phillip S. Hammond
    orcid: 0000-0002-2381-8302
    affiliation: 4
  - name: Dennis Heinemann
    orcid: 0000-0002-1434-2445
    affiliation: 5
  - name: Kristy J. Long
    orcid: 0000-0001-6970-0935
    affiliation: 6
  - name: Jeffrey Moore
    orcid: 0000-0003-3715-7442
    affiliation: 7
  - name: Maritza Sepúlveda
    orcid: 0000-0002-1403-176X
    affiliation: 8
  - name: Randall R. Reeves
    orcid: 0000-0002-6512-6507
    affiliation: 9
  - name: Guðjón Már Sigurðsson
    orcid: 0000-0001-9390-6693
    affiliation: 10
  - name: Gisli Vikingsson
    orcid: 0000-0002-4501-193X
    affiliation: 10
  - name: Paul A. Wade
    orcid: 0000-0003-2428-9323
    affiliation: 11
  - name: Rob Williams
    orcid: 0000-0001-7496-453X
    affiliation: 12
  - name: Alexandre N. Zerbini
    orcid: 0000-0002-9776-6605
    affiliation: 13
affiliations:
 - name: Resource Assessment and Conservation Engineering Division, Alaska Fisheries Science Center, National Oceanic and Atmospheric Administration, Seattle, WA, 98115, USA
   index: 1
 - name: School of Aquatic and Fishery Sciences, University of Washington, 1122 NE Boat St, Seattle, WA 98115
   index: 2
 - name: Puget Sound Institute, University of Washington Tacoma, 326 East D Street, Tacoma, WA 98421, USA
   index: 3
 - name: Sea Mammal Research Unit, Scottish Oceans Institute, University of St Andrews, Fife KY16 8LB, UK
   index: 4
 - name: U.S. Marine Mammal Commission, 4340 East-West Hwy, Rm 700, Bethesda, MD 20814, USA
   index: 5
 - name: Office of Protected Resources, NOAA's National Marine Fisheries Service, Silver Spring, MD 20910, USA
   index: 6
 - name: Protected Resources Division, NOAA SWFSC, La Jolla, CA 92037, USA
   index: 7
 - name: Facultad de Ciencias, Universidad de Valparaíso, Gran Bretaña 1111, Playa Ancha, Valparaíso, Chile
   index: 8
 - name: Okapi Wildlife Associates, Hudson, Quebec, Canada
   index: 9
 - name: Marine and Freshwater Research Institute, Skúlagata 4, 121, Reykjavík, Iceland
   index: 10
 - name: Marine Mammal Laboratory, NOAA AFSC, Seattle, WA, 98115-6349, USA
   index: 11
 - name: Oceans Initiative, 117 E. Louisa Street No. 135. Seattle, WA 98102, USA
   index: 12
 - name: Cascadia Research Collective, 218 ½ 4th Ave W, Olympia, WA, 98501, USA
   index: 13
 - name: Marine Ecology and Telemetry Research, 2468 Camp McKenzie Tr NW, Seabeck, WA, 98380, USA
   index: 14
   
date: 26 January 2022
bibliography: paper.bib
---

## Statement of Need
Fisheries bycatch is one of the top threats to marine mammals worldwide. Data on bycatch and marine mammal abundance are required to make management decisions, yet many marine mammal populations exist in locations where these data are sparse. Population models offer a way to explore the long-term impacts of different bycatch scenarios even when data are sparse or absent. Our modeling tool, `mmrefpoints`, uses very basic information about marine mammal life history, estimates of abundance and bycatch, and population models to project long-term (e.g. 20-100 yr) outcomes of different bycatch rates. These long-term outcomes are the basis of several reference points used for marine mammal management including the Potential Biological Removal (PBR) approach [@wade_calculating_1998]. The goal is to make complex population models accessible to managers and stakeholders who need them, and to students who are learning about risk-based approaches to managing marine mammal populations.

## Summary
This tool provides a way for managers and other stakeholders to explore bycatch scenarios, based on simple information about marine mammal life history and rough estimates of abundance and bycatch. The tool consists of an R package and a Shiny application. The primary machinery in the package is an age-structured population dynamics model, which is used to model future population size based on the current population size, life history traits, and bycatch rates. The package also contains tools for calculating performance metrics, as well as the U.S. reference point for bycatch (Potential Biological Removal or PBR) and a solver that estimates the maximum bycatch rate that will meet management objectives. For users who would prefer to see outputs in an interactive user interface, there is a Shiny app called the Marine Mammal Bycatch Impacts Exploration Tool (MMBIET) that shows projections, explains and calculates reference points, and creates a report summarizing inputs and outputs. The app is hosted publicly on the web via the [shinyapps.io server](https://msiple.shinyapps.io/mmrefpoints/), or it can be run locally by installing the package and running the `run_app()` function in the R or RStudio console:

```{r}
remotes::install_github("mcsiple/mmrefpoints")
library(mmrefpoints)
run_app()
```

## Population model
The population model is a simplified version of the model described in Breiwick et al. [-@breiwick_population_1984] and Punt [-@punt_a._e._annex_1999]. The model is a single-sex, age-structured population model. The number of calves or pups born each year is density-dependent, based on the number of mature adults, the pregnancy rate at pre-exploitation equilibrium, maximum theoretical fecundity rate, degree of compensation, and the total abundance relative to carrying capacity K. 
User inputs determine the parameters for calf survival, adult survival, age at maturity, and plus group age. Default parameters are based on those used by Punt et al. [-@punt_conserving_2018], with additional default parameters for other species based drawn from literature values (Arso Civil et al. [-@arso_civil_variations_2019], Ólafsdóttir et al. [-@olafsdottir_growth_2003], and Speakman et al. [-@speakman_mark-recapture_2010]).  Because of the variation in pinniped life history parameters, parameter estimates for several pinniped species are provided within the Shiny app. These life history parameters are taken from Butterworth et al. [-@butterworth_effects_1995], DeLong et al. [-@delong_age-_2017], Dillingham et al. [-@dillingham_improved_2016], Hastings et al. [-@hastings_sex-_2012], and Moore [-@moore_unpublished_2019].

The full model description including equations is contained in the “Model description” vignette and the “About the Model” tab of the app.

## Intended use
### `mmrefpoints` and the MMBIET app are intended to be used for the following:

* Exploring outcomes for bycatch rates in populations with little or no information
* Calculating reference points like PBR to estimate maximum allowable bycatch for marine mammal populations

### The MMBIET app is *not* intended for the following:

* Permitting specific management actions regarding bycatch. We refer users to Hammond et al. [-@hammond_estimating_2021], Moore et al. [-@moore_estimating_2021], and Wade et al. [-@wade_best_2021] for guidance on developing a management program for marine mammal bycatch including monitoring.
* Calculating PBR for marine mammal stocks that already have a stock assessment. If reference points have already been calculated for the stock, those should be used.
* Fitting population models to data (we direct readers to other tools like [rSPAMM](https://github.com/NorskRegnesentral/rSPAMM) for this type of need)

## Acknowledgements
The authors would like to thank several pilot testers for reviewing a beta version of the MMBIET Shiny app, and Christine Stawitz and Jay Barlow for reviewing an earlier version of the `mmrefpoints` R package and the app.

## Ongoing projects using MMBIET
At the time of this submission, three papers have cited `mmrefpoints` and/or MMBIET:

Hammond, P. S., Francis, T. B., Heinemann, D., Long, K. J., Moore, J. E., Punt, A. E., Reeves, R. R., Sepúlveda, M., Sigurðsson, G. M., Siple, M. C., Víkingsson, G., Wade, P. R., Williams, R., & Zerbini, A. N. (2021). Estimating the Abundance of Marine Mammal Populations. Frontiers in Marine Science, 8, 1316. https://doi.org/10.3389/fmars.2021.735770

Moore, J. E., Heinemann, D., Francis, T. B., Hammond, P. S., Long, K. J., Punt, A. E., Reeves, R. R., Sepúlveda, M., Sigurðsson, G. M., Siple, M. C., Víkingsson, G. A., Wade, P. R., Williams, R., & Zerbini, A. N. (2021). Estimating Bycatch Mortality for Marine Mammals: Concepts and Best Practices. Frontiers in Marine Science, 8, 1793. https://doi.org/10.3389/fmars.2021.752356

Wade, P. R., Long, K. J., Francis, T. B., Punt, A. E., Hammond, P. S., Heinemann, D., Moore, J. E., Reeves, R. R., Sepúlveda, M., Sullaway, G., Sigurðsson, G. M., Siple, M. C., Víkingsson, G. A., Williams, R., & Zerbini, A. N. (2021). Best Practices for Assessing and Managing Bycatch of Marine Mammals. Frontiers in Marine Science, 8, 1566. https://doi.org/10.3389/fmars.2021.757330


## Financial support
Support for this project is provided by the Ocean Modeling Forum and the Lenfest Ocean Program. M. Siple was supported by the Ocean Modeling Forum and a James S. McDonnell Postdoctoral Fellowship.

## References

