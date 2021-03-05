#' Calculate the derivative of C in getMNPL()
#'
#' @description this is the function to minimize, but needs to be restricted to that yield > 0
#' All life history params are as above
#' @param logit.E logit transform of bycatch mortality
#' @param S0 calf/pup survival
#' @export
get_dc <- function(logit.E, S0 = S0.w, S1plus = S1plus.w, nages = nages.w, AgeMat = AgeMat.w, lambdaMax = lambdaMax.w, K1plus = K1plus.w, z = z.w) {
  exploitation.rate <- inv.logit(logit.E)
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
    K1plus = K1plus,
    AgeMat = AgeMat,
    z = z,
    lambdaMax = lambdaMax,
    E = exploitation.rate + h,
    A = A, P0 = P0, N0 = N0
  ) / (2 * h)
  C2 <- ce(
    S0 = S0,
    S1plus = S1plus,
    nages = nages,
    K1plus = K1plus,
    AgeMat = AgeMat,
    z = z,
    lambdaMax = lambdaMax,
    E = exploitation.rate - h,
    A = A, P0 = P0, N0 = N0
  ) / (2 * h)

  dC <- C1 - C2 # diff
  return(dC)
}
