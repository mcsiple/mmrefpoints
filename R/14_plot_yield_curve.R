#
#' Yield curve for Shiny
#'
#' @param lh.params a list of life history parameters
#' @param z a vector of degrees of compensation that you want to plot. Default is the user-defined z, calculated from \code{MNPL_in}
#' @param MNPL_in the value of Maximum Net Productivity Level (MNPL) that results in
#'
#' @return
#' @export
#'
#' @examples
#' plot_yield_curve(lh.params = list(S0 = 0.944, S1plus = 0.99, AgeMat = 17, 
#' nages = 19,  fmax = 0.29, z = 2.39, lambdaMax = 1.04, K1plus = 9000), 
#' z = 2.39, MNPL_in = 0.5, lang = "en")
plot_yield_curve <- function(lh.params, z, MNPL_in, lang = "en") {
  p1 <- pop_vs_yield(z.vec = z, lh.params = lh.params, ggp = TRUE, lang = lang)
  p2 <- p1 + geom_vline(xintercept = MNPL_in, colour = "grey", lty = 2, lwd = 1.1)
  return(p2)
}
