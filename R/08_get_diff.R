#' Calculate the derivative of C in getMNPL()
#'
#' @description this is the function to minimize, but needs to be restricted to that yield > 0
#' All life history params are as above
#'
#' @param logit.E logit transform of bycatch mortality
#' @param S0 Calf/pup survival, a numeric value between 0 and 1
#' @param S1plus adult survival
#' @param nages Plus group age in years
#' @param AgeMat Age at maturity in years (must be equal to or less than nages)
#' @param lambdaMax Maximum theoretical population growth rate
#' @param K1plus Carrying capacity in terms of the age 1+ component of the population
#' @param z Pella-Tomlinson parameter (compensation)

#' @examples
#' get_diff(
#'   logit.E = logit(0.01),
#'   S0 = 0.944, S1plus = 0.99,
#'   nages = 13, AgeMat = 11,
#'   lambdaMax = 1.04,
#'   K1plus = 9000,
#'   z = 2.39
#' )
#' @export
get_diff <- function(logit.E, S0 = S0.w, S1plus = S1plus.w, nages = nages.w, AgeMat = AgeMat.w, lambdaMax = lambdaMax.w, K1plus = K1plus.w, z = z.w) {
  exploitation.rate <- inv_logit(logit.E)
  h <- 0.000001

  NPROut <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = 0)
  N0 <- NPROut$npr
  P0 <- NPROut$P1r
  Fec0 <- 1.0 / N0
  fmax <- getfecmax(lambdaMax = lambdaMax, S0 = S0, S1plus = S1plus, AgeMat = AgeMat)
  A <- (fmax - Fec0) / Fec0

  # approximate the derivative by evaluating cE() at a slightly higher and slightly lower E
  C1 <- ce(
    S0 = S0,
    S1plus = S1plus,
    nages = nages,
    AgeMat = AgeMat,
    z = z,
    E = exploitation.rate + h,
    A = A, P0 = P0, N0 = N0
  ) / (2 * h)
  
  C2 <- ce(
    S0 = S0,
    S1plus = S1plus,
    nages = nages,
    AgeMat = AgeMat,
    z = z,
    E = exploitation.rate - h,
    A = A, P0 = P0, N0 = N0
  ) / (2 * h)

  dC <- C1 - C2 # diff
  return(dC)
}
