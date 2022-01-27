#' Calculate maximum theoretical fecundity rate
#'
#' @description Calculate maximum theoretical fecundity rate \emph{fmax}
#' @details Parts of this derivation can be found in Breiwick et al. (1984) and Butterworth and Punt (1992).
#' \emph{Important}: when applying this calculation, use age at maturity ( \code{AgeMat} )
#'
#' @param S0 calf/pup survival, a numeric value between 0 and 1
#' @param lambdaMax maximum population growth rate (must exceed 0; default value is 1.04 for cetaceans and 1.12 for pinnipeds)
#' @param S1plus survival of age 1+ individuals, a numeric value between 0 and 1
#' @param AgeMat age at maturity in years (must be equal to or less than \code{nages})
#'
#' @return a numeric value for maximum theoretical fecundity.
#'
#' @examples
#' # This is fmax, the maximum theoretical fecundity
#' x <- getfecmax(lambdaMax = 1.04, S0 = 0.944, S1plus = 0.99, AgeMat = 17)
#' # This is fec0, the fecundity when there is no bycatch mortality (only M)
#' unpr <- npr(S0 = 0.944, S1plus = 0.99, AgeMat = 17, nages = 10000, E = 0)
#' print(x)
#' print(1 / unpr$npr)
#' @export
getfecmax <- function(S0, lambdaMax, S1plus, AgeMat) {
  if(S0 < 0 | S0 >= 1){stop("Calf/pup survival must be between 0 and 1.")}
  if(S1plus < 0 | S1plus >= 1){stop("Adult survival must be between 0 and 1.")}
  
  fmax <- (lambdaMax^(AgeMat) - (S1plus * (lambdaMax^(AgeMat - 1)))) / (S0 * S1plus^(AgeMat - 1))
  return(fmax)
}
