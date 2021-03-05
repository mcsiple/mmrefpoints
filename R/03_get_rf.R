#' Get recruitment at bycatch mortality rate E
#'
#' @description calculates recruitment at bycatch mortality rate \emph{E}
#'
#' @param E initial bycatch mortality rate (numeric value)
#' @param S0 calf or pup survival (numeric value)
#' @param S1plus adult survival  (numeric value)
#' @param nages number of age classes-- including plus group age-- in years
#' @param K1plus adult carrying capacity (number of individuals)
#' @param AgeMat age at maturity in years
#' @param z degree of compensation, in the app calculated from the value of MNPL defined by the user
#' @param A Pella-Tomlinson resilience parameter (see Punt 1999; Annex R).
#' A = (FecMax - Fec0) / Fec0 (num)
#' @param P0 unfished 1+ numbers per recruit, \eqn{tildeP(0)}
#' @param N0 unfished mature numbers per recruit, \eqn{tildeN(0)}
#'
#' @return recruitment given exploitation rate \emph{E} - this value is multiplied by the initial abundance \eqn{N_{init}} to get initial nums at age ( a vector)
#'
#' @export
get_rf <- function(E_in, S0, S1plus, nages, K1plus, AgeMat, z, A, P0, N0) {
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
