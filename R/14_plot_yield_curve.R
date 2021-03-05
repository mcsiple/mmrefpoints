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
plot_yield_curve <- function(lh.params, z, MNPL_in, lang = "en") {
  p1 <- pop_vs_yield(z.vec = z, lh.params = lh.params, ggp = TRUE, lang = lang)
  p2 <- p1 + geom_vline(xintercept = MNPL_in, colour = "grey", lty = 2, lwd = 1.1)
  return(p2)
}
