#' Degree of compensation
#'
#' @description Calculate the parameter z, the degree of compensation
#' @param MNPL_in User-specified value for MNPL (between 0 and 1)
#' @param lh.params_in a list of life history parameters
#' @details Helper function for calculating z when user specifies MNPL
#'
#' @return the value of z corresponding to the value of MNPL (as a proportion of K) that the user has defined.
#' @export
#'
#' @examples
#' test.z <- calc_z(MNPL_in = 0.4, lh.params_in = lh.params1)
calc_z <- function(MNPL_in, lh.params_in) {
  lims <- c(0.107, 7) # z limits from AEP meeting were 0 and 7; I increased the lower bound because too-low z is a problem
  zero.cross <- tryCatch(
    uniroot(f = get_dz, interval = lims, tol = 1e-7, MNPL = MNPL_in, lh.params = lh.params_in),
    error = function(e) {
      "x"
    }
  )
  if (is.character(zero.cross)) {
    stop(safeError("Error in solving for z. Try a smaller value of MNPL."))
  }
  z.out <- zero.cross$root
  return(z.out)
}
