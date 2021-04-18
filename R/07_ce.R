#' Calculate normalized sustainable yield
#' @description This function calculates the normalized sustainable yield, which is used to find MNPL (the population size at which productivity is maximized).
#'
#' @param S0 Calf/pup survival, a numeric value between 0 and 1
#' @param S1plus 1+ survival rate for animals age 1 year and older, a numeric value between 0 and 1
#' @param AgeMat Age at maturity (= age at first parturition - 1). Must be less than \code{nages}
#' @param nages "maximum" age, treated as the plus group age. The plus group age can be set equal to the age at maturity +2 years without losing accuracy.
#' @param z degree of compensation
#' @param E bycatch mortality rate (applies to 1+ numbers)
#' @param A the Pella-Tomlinson resilience parameter ((fmax - f0)/f0)
#' @param P0 unfished number-per-recruit - 1+ adults
#' @param N0 unfished numbers-per-recruit - mature adults
#'
#' @return a single value of normalized yield for exploitation rate E
#'
#' @examples
#' Set parameters
#' S0.w = 0.5; S1plus.w = 0.944; nages.w = 25; AgeMat.w = 18 
#' # Get number of individuals per recruit in terms of mature individuals (N0.w)
#' NPROut <- npr(S0 = S0.w, S1plus = S1plus.w, nages = nages.w, AgeMat = AgeMat.w, E = 0)
#' 
#' N0 <- NPROut$npr # mature numbers per recruit
#' # Get number of individuals per recruit in terms of individuals aged 1+ (P0.w)
#' P0 <- NPROut$P1r # 1+ nums per recruit
#'
#' ce(S0 = S0.w, S1plus = S1plus.w, 
#' nages = nages.w, 
#' AgeMat = AgeMat.w, 
#' E=0.01, z=2.39,A=2, N0 = N0, P0 = P0)
#' @export
ce <- function(S0 = NA, S1plus = NA, AgeMat = NA, nages = NA, z = NA, E = NA, A = NA, P0 = NA, N0 = NA) {
  if(AgeMat > nages){warning("Age at maturity cannot be larger than plus group age. Change AgeMat or nages.")}
  if(S0 < 0 | S0 >= 1){stop("Calf/pup survival must be between 0 and 1.")}
  if(S1plus < 0 | S1plus >= 1){stop("Adult survival must be between 0 and 1.")}
  if(E > 1 | E < 0){warning("Check E input: bycatch mortality rate E should be between 0 and 1")}
  
  npr1plus <- npr(
    S0 = S0,
    S1plus = S1plus,
    nages = nages,
    AgeMat = AgeMat,
    E = E
  )$P1r # 1+ nums per recruit @ E, written as P[tilde](E)
  recatF <- get_rf(
    E_in = E,
    S0 = S0,
    S1plus = S1plus,
    nages = nages,
    AgeMat = AgeMat,
    z = z,
    A = A,
    P0 = P0,
    N0 = N0
  ) # recruitment at E
  
  cpr1plus <- E * recatF * npr1plus # 1+ catch-per-recruit at bycatch mortality rate E, AKA normalized sustainable yield. The middle term is normalized recruitment at E, known as B(E) in some other formulations. It is recatF/recat0, where recat0 is recruitment at E = 0 (no bycatch) -- but because our calculation is per-recruit, recat0 = 1 and the relative recruitment is just recatF.
  return(cpr1plus)
}
