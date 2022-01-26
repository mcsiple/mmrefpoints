#' Extract results from `dynamics()` function as a dataframe
#'
#' @description #unnest results from dynamics() and turn them into a dataframe that you can bind together
#' @param x Outputs from a call to projections(), which are a named list
#'
#' @return a dataframe with population trajectories, initial depletion, and bycatch levels that can be printed as a table.
#' @export
#'
#' @examples
#' x <- projections(
#'   NOut = 3, ConstantRateBycatch = list(Rate = 0.01, CV = 0.3),
#'   InitDepl = 0.8,
#'   lh.params = list(
#'     S0 = 0.944, S1plus = 0.99,
#'     K1plus = 9000, AgeMat = 18, nages = 20, z = 2.39, lambdaMax = 1.02
#'   ),
#'   nyears = 50, obs_CV = 0
#' )
#' extract_df(x)
extract_df <- function(x) {
  y <- data.frame(round(x$trajectories, digits = 0))
  colnames(y) <- paste0("Yr_", 1:ncol(y))
  rownames(y) <- 1:nrow(y)

  y$sim <- 1:nrow(y)

  y$Bycatch_Catch <- x$ConstantBycatch["Catch"]
  y$Bycatch_CV <- x$ConstantBycatch["CV"]

  y$Bycatch_Rate <- x$ConstantRateBycatch["Rate"]
  y$Bycatch_Rate_CV <- x$ConstantRateBycatch["CV"]

  y$InitDepl <- x$InitDepl

  return(y)
}
