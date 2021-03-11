#' Get relative recruitment at bycatch mortality rate E
#'
#' @description calculates recruitment at bycatch mortality rate \emph{E}, relative to a no-bycatch scenario
#' @param S0 calf or pup survival (numeric value)
#' @param S1plus adult survival  (numeric value)
#' @param nages number of age classes-- including plus group age-- in years
#' @param AgeMat age at maturity in years
#' @param z degree of compensation, in the app calculated from the value of MNPL defined by the user
#' @param A Pella-Tomlinson resilience parameter (see Punt 1999; Annex R).
#' A = (FecMax - Fec0) / Fec0 (num)
#' @param P0 unfished 1+ numbers per recruit, \eqn{tildeP(0)}
#' @param N0 unfished mature numbers per recruit, \eqn{tildeN(0)}
#' @param E_in Bycatch mortality rate
#'
#' @return recruitment given exploitation rate \emph{E} - this value is multiplied by the initial abundance \eqn{N_{init}} to get initial nums at age ( a vector)
#'
#' @examples 
#' S0 = 0.944; S1plus = 0.99; nages = 25; AgeMat = 17; z = 2.39; lambdaMax = 1.04;
#' NPROut <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = 0)
#' N0 <- NPROut$npr # mature numbers per recruit
# Get number of individuals per recruit in terms of individuals aged 1+ (\eqn{P0.w})
#' P0 <- NPROut$P1r # 1+ nums per recruit
#' Fec0 <- 1.0 / N0
#' FecMax <- getfecmax(S1plus = S1plus, S0 = S0, AgeMat = AgeMat, lambdaMax = lambdaMax)
#' A <- (FecMax - Fec0) / Fec0
#' get_rf(E_in = 0.01, S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, z = z, A = A,P0 = P0,N0 = N0)
#' 
#' @export
get_rf <- function(E_in, S0, S1plus, nages, AgeMat, z, A, P0, N0) {
  NE <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = E_in)$npr
  PE <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = E_in)$P1r

  # More general version ('actual' R0 = K1plus/P1r)
  R0 <- 1

  # Fecundity at unfished equilibrium
  Fec0 <- 1.0 / N0

  if ((1 - Fec0 * NE) / (Fec0 * NE * A) > 1) {
    R.F <- 0
  } else {
    R.F <- (1 - (1 - Fec0 * NE) / (Fec0 * NE * A))^(1 / z) * ((R0 * P0) / PE)
  }

  return(R.F)
}
