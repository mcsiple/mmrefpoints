#' Calculate the probability of reaching a rebuilding goal
#'
#' @description - a simple function that calculates the probability of recovery to MNPL (or other) after rebuilt.yr years
#'
#' @param traj - a matrix of trajectories, with rows=nsims and cols=nyears
#' @param goal - what the rebuild goal is-- right now it is max net productivity level (MNPL), the pop size at which max population growth occurs. usually ~0.5-0.6 K for whales.
#' @param rebuild.yr - the year by which you want pop to be recovered (calculate probability that pop will recover to MNPL by rebuild.yr)
#'
#' @return - the probability that each stock will be rebuilt to MNPL by year rebuilt.yr
#' @export
prob_rebuilt_goal <- function(traj, goal = 0.5 * lh.params.test$K1plus, rebuild.yr = 100) {
  if (ncol(traj) < rebuild.yr) stop("Need more years of projection")
  nsims <- nrow(traj)
  prob <- length(which(traj[, rebuild.yr] > goal)) / nsims
  prob <- round(prob, digits = 2)
  return(prob)
}
