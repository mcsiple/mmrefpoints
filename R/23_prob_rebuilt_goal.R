#' Calculate the probability of reaching a rebuilding goal
#'
#' @description - a simple function that calculates the probability of recovery (in terms of 1+ numbers) to MNPL (or other population size) at \code{rebuilt.yr} years
#'
#' @param traj - a matrix of trajectories, with rows=nsims and cols=nyears
#' @param goal - what is the rebuilding goal? An absolute number.
#' @param rebuild.yr - the year by which you want pop to be recovered (calculate probability that pop will recover to MNPL by rebuild.yr)
#'
#' @return - the probability that the stock will be rebuilt to the goal population size by year \code{rebuilt.yr}
#' 
#' @examples 
#' #' lh.params = list(S0 = 0.944, S1plus = 0.99, AgeMat = 17, 
#'  z = 2.39, lambdaMax = 1.04, K1plus = 9000)
#' Projection <- projections(NOut=100,
#' ConstantBycatch = list(Catch = 200, CV = 01),
#' InitDepl=0.8,
#' lh.params=lh.params,nyears=40,obs_CV = 0.2) 
#' print(prob_rebuilt_goal(Projection$trajectories, goal = 0.5 * lh.params$K1plus, rebuild.yr = 40))
#' 
#' @export
prob_rebuilt_goal <- function(traj, goal = 2000, rebuild.yr = 100) {
  if (ncol(traj) < rebuild.yr) stop("Need more years of projection")
  nsims <- nrow(traj)
  prob <- length(which(traj[, rebuild.yr] > goal)) / nsims
  prob <- round(prob, digits = 2)
  return(prob)
}
