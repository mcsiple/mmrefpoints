# mmrefpoints: Projecting long-term marine mammal abundance with bycatch

## Statement of Need
Fisheries bycatch is one of the top threats to marine mammals worldwide. Data on bycatch and marine mammal abundance are required to make management decisions, yet many marine mammal populations exist in locations where these data are sparse. Population models offer a way to explore the long-term impacts of different bycatch scenarios even when data are sparse or absent. Our modeling tool, `mmrefpoints`, uses very basic information about marine mammal life history, estimates of abundance and bycatch, and population models to project long-term (e.g. 20-100 yr) outcomes of different bycatch rates. These long-term outcomes are the basis of several reference points used for marine mammal management including the Potential Biological Removal (PBR) approach (Wade 1998). The goal is to make complex population models accessible to managers and stakeholders who need them, and to students who are learning about risk-based approaches to managing marine mammal populations.

## Summary
This tool provides a way for managers and other stakeholders to explore bycatch scenarios, based on simple information about marine mammal life history and rough estimates of abundance and bycatch. The tool consists of an R package and a Shiny application. The primary machinery in the package is an age-structured population dynamics model, which is used to model future population size based on the current population size, life history traits, and bycatch rates. The package also contains tools for calculating performance metrics, as well as the U.S. reference point for bycatch (Potential Biological Removal or PBR) and a solver that estimates the maximum bycatch rate that will meet management objectives. For users who would prefer to see outputs in an interactive user interface, there is a Shiny app called the Marine Mammal Bycatch Impacts Exploration Tool (MMBIET) that shows projections, explains and calculates reference points, and creates a report summarizing inputs and outputs. The app is hosted publicly on the web via the [shinyapps.io server](https://msiple.shinyapps.io/mmrefpoints/), or it can be run locally by installing the package and running the `run_app()` function in the R or RStudio console:

```{r}
remotes::install_github("mcsiple/mmrefpoints")
mmrefpoints::run_app()
```

## Population model
The population model is a simplified version of the model described in Breiwick et al. (1984) and Punt (1999). The model is a single-sex, age-structured population model. The number of calves or pups born each year is density-dependent, based on the number of mature adults, the pregnancy rate at pre-exploitation equilibrium, maximum theoretical fecundity rate, degree of compensation, and the total abundance relative to carrying capacity K. 
User inputs determine the parameters for calf survival, adult survival, age at maturity, and plus group age. Default parameters are based on those used by Punt et al. (2018), with additional default parameters for other species based drawn from literature values (Arso et al. [2019], Ólafsdóttir et al. [2003], and Speakman et al. [2010]).  Because of the variation in pinniped life history parameters, parameter estimates for several pinniped species are provided within the Shiny app. These life history parameters are taken from Butterworth et al. (1995), DeLong et al. (2017), Dillingham et al. (2016), Hastings et al. (2012), and Moore (2019).

The full model description including equations is contained in the “Model description” vignette and the “About the Model” tab of the app.

## Intended use
### `mmrefpoints` and the MMBIET app are intended to be used for the following:

* Exploring outcomes for bycatch rates in populations with little or no information
* Calculating reference points like PBR to estimate maximum allowable bycatch for marine mammal populations

### The MMBIET app is *not* intended for the following:

* Permitting specific management actions regarding bycatch. We refer users to Hammond et al. (2021), Moore et al. (n.d.), and Wade et al. (2021) for guidance on developing a management program for marine mammal bycatch including monitoring.
* Calculating PBR for marine mammal stocks that already have a stock assessment. If reference points have already been calculated for the stock, those should be used

## Acknowledgements
The authors would like to thank several pilot testers for reviewing a beta version of the MMBIET Shiny app, and Christine Stawitz for reviewing an earlier version of the `mmrefpoints` R package and the app.

## Ongoing projects using MMBIET
At the time of this submission, three papers have cited `mmrefpoints` and/or MMBIET:

Published:

Hammond, P. S., Francis, T. B., Heinemann, D., Long, K. J., Moore, J. E., Punt, A. E., Reeves, R. R., Sepúlveda, M., Sigurðsson, G. M., Siple, M. C., Víkingsson, G., Wade, P. R., Williams, R., & Zerbini, A. N. (2021). “Estimating the Abundance of Marine Mammal Populations.” Frontiers in Marine Science, 8, 735770. https://doi.org/10.3389/fmars.2021.735770

Accepted for publication:

Moore, J. E., Heinemann, D., Francis, T. B., Hammond, P. S., Long, K. J., Punt, A. E., Reeves, R. R., Sepulveda, M., Sigurðsson, G. M., Siple, M. C., Vikingsson, G., Wade, P. R., Williams, R., & Zerbini, A. N. (n. d.). “Estimating bycatch mortality for marine mammal stock assessment: Concepts and best practices.” *Accepted for publication in Frontiers in Marine Science.*

Wade, P. R., Long, K. J., Francis, T. B., Punt, A. E., Hammond, P. S., Heinemann, D., Moore, J. E., Reeves, R. R., Sepulveda, M., Sullaway, G., Sigurðsson, G. M., Siple, M. C., Vikingsson, G., Williams, R., & Zerbini, A. N. (n. d.). “Best practices for assessing and managing bycatch of marine mammals.” *Accepted for publication in in Frontiers in Marine Science.*

## References

Arso Civil, M., B. Cheney, N. J. Quick, V. Islas-Villanueva, J. A. Graves, V. M. Janik, Paul M. Thompson, P. S. Hammond. 2019. “Variations in age- and sex-specific survival rates help explain population trend in a discrete marine mammal population.” Ecology and Evolution 9 (1): 533–44. https://doi.org/10.1002/ece3.4772.

Breiwick, Jeffrey M., L. Lee Eberhardt, and Howard W. Braham. 1984. “Population dynamics of western Arctic bowhead whales (*Balaena mysticetus*).” Canadian Journal of Fisheries and Aquatic Sciences 41 (3): 484–96. https://doi.org/10.1139/f84-058.

Butterworth, D. S., A. E. Punt, W. H. Oosthuizen, and P. A. Wickens. 1995. “The Effects of future consumption by the Cape fur seal on catches and catch rates of the Cape hakes. 3. Modelling the dynamics of the Cape fur seal *Arctocephalus pusillus pusillus*.” South African Journal of Marine Science 16 (1): 161–83. https://doi.org/10.2989/025776195784156511.

DeLong, Robert L., Sharon R. Melin, Jeffrey L. Laake, Patricia Morris, Anthony J. Orr, and Jeffrey D. Harris. 2017. “Age- and sex-specific survival of California sea lions (*Zalophus californianus*) at San Miguel Island, California.” Marine Mammal Science 33 (4): 1097–1125. https://doi.org/10.1111/mms.12427.

Dillingham, Peter W., Jeffrey E. Moore, David Fletcher, Enric Cortés, K. Alexandra Curtis, Kelsey C. James, and Rebecca L. Lewison. 2016. “Improved estimation of intrinsic growth Rmax for long-lived species: Integrating matrix models and allometry.” Ecological Applications 26 (1): 322–33. https://doi.org/10.1890/14-1990.

Hammond, P. S., T. B. Francis, D. Heinemann, K. J. Long, J. E. Moore, A. E. Punt, R. R. Reeves, et al. n.d. “Estimating the abundance of marine mammal populations.”

Hastings, Kelly K., Robert J. Small, and Grey W. Pendleton. 2012. “Sex- and age-specific survival of harbor seals (*Phoca vitulina*) from Tugidak Island, Alaska.” Journal of Mammalogy 93 (5): 1368–79. https://doi.org/10.1644/11-MAMM-A-291.1.

Moore, J. E. 2019. Unpublished estimates of life history parameters following the methods of Dillingham et al. 2016.

Moore, J. E., D. Heinemann, T. B. Francis, P. S. Hammond, K. J. Long, A. E. Punt, R. R. Reeves, et al. n.d. “Estimating bycatch mortality for marine mammal stock assessment: Concepts and best practices.”

Ólafsdóttir, Droplaug, Gísli A. Víkingsson, Sverrir Daníel Halldórsson, and Jóhann Sigurjónsson. 2003. “Growth and reproduction in harbour porpoises (*Phocoena phocoena*) in Icelandic waters.” NAMMCO Scientific Publications 5 (July): 195–210. https://doi.org/10.7557/3.2747.

Punt, A. E. 1999. “Annex R: A Full Description of the Standard Baleen II Model and Some Variants Thereof.” Hobart, Australia: Division of Marine Research, CSIRO Marine Laboratories 

Punt, A. E., P. Moreno, J. R. Brandon, and Michael A Mathews. 2018. “Conserving and recovering vulnerable marine species: a Comprehensive evaluation of the US approach for marine mammals.” ICES Journal of Marine Science 75 (5): 1813–31. https://doi.org/10.1093/icesjms/fsy049.

Speakman, T. R., Lane, S. M., Schwacke, L. H., Fair, P.A., and Zolman, E. S. 2010. “Mark-recapture estimates of seasonal abundance and survivorship for bottlenose dolphins (*Tursiops truncatus*) Near Charleston, South Carolina, USA,” 11.

Wade, P. R. 1998. Calculating limits to the allowable human-caused mortality of cetaceans and pinnipeds. Marine Mammal Science 14(1): 1–37. https://doi.org/10.1111/j.1748-7692.1998.tb00688.x.

Wade, P. R., K. J. Long, T. B. Francis, A. E. Punt, P. S. Hammond, D. Heinemann, J. E. Moore, et al. n.d. “Best practices for assessing and managing bycatch of marine mammals.”

