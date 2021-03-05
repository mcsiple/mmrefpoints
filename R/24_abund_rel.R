#' Calculate relative abundance
#' @description Calculates the expected abundance relative to zero exploitation, years.vec years after projections start
#'
#' @param traj a matrix of trajectories, with rows=nsims and cols=nyears
#' @param zero.traj a matrix of trajectories, same as traj but unfished. CHECK to make sure both start at the same simulation, in the app code.
#' @param K if calculating abundance relative to K, put in K.
#' @param fulldist logical saying whether to return the full distribution of relative abundances or the median of all the relative abundances.
#' @param years.vec a vector of years to check abundances at. E.g., how many years after the start of projections do you want to know abundance?
#'
#' @return a vector of abundance relative to K or zero-exploitation where nrows=length of years.vec (10 years after, 20 years, etc.) and ncol=number of simulations.
#'
#' @export
abund_rel <- function(traj, zero.traj, K = NA, years.vec = c(10, 20, 50), fulldist = TRUE) {
  nsims <- nrow(traj)
  rel.abund <- matrix(data = NA, nrow = length(years.vec), ncol = nsims)
  if (!is.na(K)) {
    list.outputs <- lapply(X = years.vec, FUN = function(x) traj[, x] / K)
  } else {
    list.outputs <- lapply(X = years.vec, FUN = function(x) traj[, x] / zero.traj[, x])
  }
  names(list.outputs) <- paste("YrPost", years.vec, sep = "")

  if (fulldist) {
    return(list.outputs)
  } else {
    final <- lapply(X = list.outputs, FUN = median)
    final <- round(as.numeric(final), digits = 2)
    return(final)
  }
}
