#' Life history parameters for marine mammals.
#'
#' A table with the default parameter values for the marine mammals included in this app.
#'
#' @format A data frame with 11 rows and 11 variables:
#' \describe{
#'   \item{Type}{Life history type}
#'   \item{Code}{a quick reference for that life history type}
#'   \item{Representative}{The species representing that life history type}
#'   \item{S0}{Calf/pup survival}
#'   \item{S1plus}{Adult survival}
#'   \item{AgeMat}{Age at maturity}
#'   \item{nages}{Plus group age; referred to as nages in code}
#'   \item{fmax}{Maximum theoretical fecundity}
#'   \item{z}{Degree of compensation}
#'   \item{lambdaMax}{Maximum theoretical population growth rate}
#'   \item{K1plus}{Carrying capacity in terms of the 1+ component of the population}
#'   ...
#' }
#' @source Compiled from literature by M. Siple and coauthors
"lh"