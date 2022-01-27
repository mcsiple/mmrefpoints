#' Calculate the derivative of C in getMNPL()
#'
#' @description this is the function to minimize, but needs to be restricted to that yield > 0
#' All life history params are as above
#'
#' @param logit.E logit transform of bycatch mortality
#' @param S0 Calf/pup survival, a numeric value between 0 and 1
#' @param S1plus adult survival, a numeric value between 0 and 1
#' @param AgeMat Age at maturity (= age at first parturition - 1)
#' @param nages "maximum" age, treated as the plus group age. The plus group age can be set equal to the age at maturity +2 years without losing accuracy.
#' @param A the Pella-Tomlinson resilience parameter ((fmax - f0)/f0)
#' @param z Degree of compensation (also known as the Pella-Tomlinson parameter)
#' 
#' @examples
# # Set parameters
#' S0.w = 0.5; S1plus.w = 0.944; nages.w = 25; AgeMat.w = 18 
#' InitDepl.w = 0.9; z.w = 2.39; lambdaMax.w = 1.04
#' # Get A parameter
#' NPROut <- npr(S0 = S0.w, S1plus = S1plus.w, nages = nages.w, AgeMat = AgeMat.w, E = 0)
#' N0 <- NPROut$npr # mature numbers per recruit
#' Fec0 <- 1.0 / N0
#' fmax <- getfecmax(lambdaMax = lambdaMax.w, S0 = S0.w, S1plus = S1plus.w, AgeMat = AgeMat.w)
#' A.w <- (fmax - Fec0) / Fec0
#' # Get number of individuals per recruit in terms of mature individuals (\eqn{N0.w})
#' get_diff(
#'  logit.E = logit(0.01), S0 = S0.w, S1plus = S1plus.w, nages = nages.w, A = A.w, AgeMat = AgeMat.w, 
#'   z = 2.39
#' )
#' @export
get_diff <- function(logit.E, S0 = S0.w, S1plus = S1plus.w, AgeMat = AgeMat.w, nages = nages.w, A = A.w, z = z.w) {
  if(AgeMat > nages){warning("Age at maturity cannot be larger than plus group age. Change AgeMat or nages.")}
  if(S0 < 0 | S0 >= 1){stop("Calf/pup survival must be between 0 and 1.")}
  if(S1plus < 0 | S1plus >= 1){stop("Adult survival must be between 0 and 1.")}
  if(is.infinite(logit.E)){stop("Check inputs; bycatch mortality rate E must be less than 1.")}
  
  exploitation.rate <- inv_logit(logit.E)
  h <- 0.000001
  
  NPROut <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = 0)

  N0 <- NPROut$npr
  P0 <- NPROut$P1r
  
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
