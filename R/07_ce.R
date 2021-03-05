#' Calculate normalized sustainable yield
#' @description This function calculates the normalized sustainable yield, which is used to find MNPL (the population size at which productivity is maximized).
#'
#' @param S0 Calf/pup survival
#' @param S1plus 1+ survival (usually called "juvenile + adult" in literature)
#' @param nages Same as plus group age, called "maximum (lumped) age-class" in Punt (1999) Annex R
#' @param K1plus 1+ carrying capacity
#' @param AgeMat Age at maturity (= age at first parturition - 1)
#' @param z degree of compensation
#' @param E bycatch mortality rate
#' @param P0 unfished nums per recruit - 1+ adults
#' @param lambdaMax maximum steady rate of increase (population growth rate)
#' @param A the Pella-Tomlinson resilience parameter ((fmax - f0)/f0)
#' @param N0 unfished nums per recruit - mature adults
#'
#' @return a single value of normalized yield for exploitation rate E
#'
#' @export
ce <- function(S0, S1plus, nages, K1plus, AgeMat, z, lambdaMax, E, A, P0, N0) {
  npr1plus <- npr(
    S0 = S0,
    S1plus = S1plus,
    nages = nages,
    AgeMat = AgeMat,
    E = E
  )$P1r # 1+ nums per recruit @ E, written as P[tilde](E)
  recatF <- get_rf(
    E = E,
    S0 = S0,
    S1plus = S1plus,
    nages = nages,
    K1plus = K1plus,
    AgeMat = AgeMat,
    z = z,
    A = A,
    P0 = P0,
    N0 = N0
  ) # recruitment at E
  recat0 <- 1 # recruitment at E = 0 (no bycatch)
  rel_rec <- recatF / recat0 # normalized recruitment at E, known as B(E)
  cpr1plus <- E * rel_rec * npr1plus # 1+ catch-per-recruit at bycatch mortality rate E, AKA normalized sustainable yield
  return(cpr1plus)
}
