#' Calculate maximum theoretical fecundity
#'
#' @description Calculate maximum theoretical fecundity \emph{fmax}
#' @details Parts of this derivation can be found in Breiwick et al. (1984) and Butterworth and Punt (1992).
#' \emph{Important}: when applying this calculation, use age at maturity ( \code{AgeMat} ), and when defining probability of giving birth use parturition age (age at maturity + one year)
#' @param S0 calf/pup survival, a numeric value between 0 and 1
#' @param lambdaMax maximum population growth rate (default value is 1.04 for cetaceans and 1.12 for pinnipeds)
#' @param S1plus survival of age 1+ individuals, a numeric value between 0 and 1
#' @param AgeMat age at maturity (in years)
#' @return a single numeric value for maximum theoretical fecundity.
#'
#' @examples
#' x <- getfecmax(lambdaMax = 1.04, S0 = 0.944, S1plus = 0.99, AgeMat = 17)
#' unpr <- npr(S0 = 0.944, S1plus = 0.99, AgeMat = 17, nages = 10000, E = 0)
#' print(1 / unpr$npr)
#' @export
getfecmax <- function(S0, lambdaMax, S1plus, AgeMat) {
  fmax <- (lambdaMax^(AgeMat) - (S1plus * (lambdaMax^(AgeMat - 1)))) / (S0 * S1plus^(AgeMat - 1))
  return(fmax)
}
