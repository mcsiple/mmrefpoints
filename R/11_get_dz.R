#' Derivative of z (function to minimize)
#'
#' @param z the degree of compensation
#' @param MNPL maximum net productivity level
#' @param lh.params a list of life history parameters (juvenile survival S0, adult survival S1plus, age at maturity AgeMat, plus group age, max theoretical fecundity fmax, maximum steady steady rate of increase (population growth rate) lambdaMax, and carrying capacity K1plus)
#'
#' @return The difference between the MNPL associated with the value of z that the user defined and the MNPL that the user has defined.
#' @export
#'
#' @examples
#' get_dz(z = 2.39, MNPL = 0.5, lh.params = list(S0 = 0.944, S1plus = 0.99, AgeMat = 17, PlusGroupAge = 25, fmax = 0.29, lambdaMax = 1.04, K1plus = 9000))
get_dz <- function(z, MNPL, lh.params) {
  # want diff between MNPL and f(z) equal to zero
  lh.params$z <- z
  MNPL_calc <- get_mnpl(lh.params = lh.params)
  dZ <- MNPL - MNPL_calc
  return(dZ)
}
