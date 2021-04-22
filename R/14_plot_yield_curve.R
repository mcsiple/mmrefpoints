#
#' Yield curve for Shiny
#'
#' @param lh.params a list of life history parameters
#' @param z degree of compensation. If this function is used outside the Shiny app, z is calculated from MNPL.
#' @param MNPL_in Maximum Net Productivity Level (MNPL)
#' @param lang language selected by the user (character)
#'
#' @return a ggplot object showing depletion (1+ population size relative to K) vs. production. In fisheries this is a yield curve; in marine mammal management it shows where the productivity level is highest, i.e., the Maximum Net Productivity Level (MNPL).
#' @export
#'
#' @examples
#' plot_yield_curve(
#'   lh.params = list(
#'     S0 = 0.944, S1plus = 0.99, AgeMat = 17,
#'     nages = 19, z = 2.39, lambdaMax = 1.02, K1plus = 9000
#'   ),
#'   MNPL_in = 0.5, z = NA, lang = "en"
#' )
plot_yield_curve <- function(lh.params, z, MNPL_in, lang = "en") {

  # checks
  if (MNPL_in < 0 | MNPL_in > 1) {
    stop("Check inputs; MNPL should be between 0 and 1")
  }
  
  # make plots
  if (shiny::isRunning()) {
    p1 <- pop_vs_yield(z.vec = z, lh.params = lh.params, ggp = TRUE, lang = lang)
    p2 <- p1 + geom_vline(xintercept = MNPL_in, colour = "grey", lty = 2, lwd = 1.1)
    return(p2)
  } else {
    z <- calc_z(MNPL_in = MNPL_in, lh.params_in = lh.params)
    if (z < -0.5 | z > 6) {
      stop("z is out of range; try a value for MNPL that is between 0 and 1.")
    }
    print("calculating z from MNPL instead of using input; z = ", z)
    p1 <- pop_vs_yield(z.vec = z, lh.params = lh.params, ggp = TRUE, lang = lang)
    p2 <- p1 + geom_vline(xintercept = MNPL_in, colour = "grey", lty = 2, lwd = 1.1)
    return(p2)
  }
}
