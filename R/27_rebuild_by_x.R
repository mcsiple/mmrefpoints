#' Calculate the bycatch rate needed to reach a performance goal
#'
#' @description takes a performance goal (what level you want to rebuild to) and a time window (how long you want that to take) and calculates what the bycatch rate needs to be
#'
#' @param needf.start starting guess for the bycatch mortality rate needed to recover the population
#' @param init.depl.w initial depletion (a fraction)
#' @param goal.w Population size goal (number of whales or pinnipeds) to rebuild to, expressed as a whole number
#' @param desired.prob.w what probability you want (e.g., 0.75 probability that the population will rebuild to X in Y years)
#' @param when.w the year Y when rebuilding is desired by (number of years into future; current year = 0)
#' @param lh.params.w a list of life history parameters used by \code{projections()}
#' @param fixed.cv.catch.w the CV of the bycatch rate - should be fixed
#'
#' @return The bycatch rate that will result in the specified rebuilding goal. If Shiny is running, it will return a list containing the bycatch rate \code{f} and a matrix of guesses that \code{optim()} has searched through to find the solution.
#' @export
#'
#' @examples
#' rebuild_by_x(
#'   needf.start = 0.001,
#'   init.depl.w = 0.5, goal.w = 4500,
#'   desired.prob.w = 0.8, when.w = 100,
#'   lh.params.w = list(
#'     S0 = 0.944, S1plus = 0.99,
#'     AgeMat = 17, nages = 19,
#'     z = 2.39, lambdaMax = 1.04, K1plus = 9000
#'   ),
#'   fixed.cv.catch.w = 0
#' )
rebuild_by_x <- function(needf.start, init.depl.w, goal.w, desired.prob.w, when.w, lh.params.w, fixed.cv.catch.w) {

  # Checks
  if (init.depl.w < 0 | init.depl.w > 1) {
    stop("init.depl.w must be between 0 and 1")
  }
  if (desired.prob.w < 0 | desired.prob.w > 1) {
    stop("desired.prob.w must be between 0 and 1")
  }

  # Set up matrix for storing outputs while uniroot is searching
  mat <- matrix(0, 100, 2)
  pointer <- 0
  set.seed(123)

  # Step 1: function to minimize
  getprob.diff <- function(logit.rate.start,
                           init.depl = init.depl.w,
                           goal = goal.w, when = when.w,
                           lh.params = lh.params.w, desired.prob = desired.prob.w,
                           fixed.cv.catch = fixed.cv.catch.w, nyears = 100, nsims = 500) {
    # Params are the same as above-- ".w" indicates that they are parameters in the wrapper function

    fishing.rate <- inv_logit(logit.rate.start)
    traj <- projections(
      NOut = nsims, InitDepl = init.depl, lh.params = lh.params,
      ConstantRateBycatch = list(
        Rate = fishing.rate,
        CV = fixed.cv.catch
      ), nyears = 100
    )$trajectories # a matrix of trajectories, with rows=nsims and cols=nyears
    # Based on nsims simulations, this is the probability of rebuilding to the goal:
    prob <- prob_rebuilt_goal(traj = traj, goal = goal, rebuild.yr = when)

    # A "naughty" way to get the fn to save guesses globally, so they can be plotted:
    pointer <<- pointer + 1
    mat[pointer, ] <<- c(fishing.rate, prob)
    if (isRunning()) incProgress(1 / 15) # Shiny only line
    diff <- prob - desired.prob
    return(diff)
  }

  # Step 2: Find the value of F that minimizes getprob.diff()
  search.limit <- (1 - (1 / lh.params.w$lambdaMax)) * 2.5 # This prevents uniroot() from searching too far in one direction
  logit.rate.start <- logit(needf.start)

  zero.cross <- tryCatch(stats::uniroot(f = getprob.diff, interval = logit(c(0.00001, search.limit)), tol = 1e-7), error = function(c) "error")
  if (zero.cross[1] == "error") {
    return("Sorry, recovery is not possible at these starting conditions. Try a longer recovery horizon or lower recovery goal")
  } else {
    f <- inv_logit(zero.cross$root)

    if (isRunning()) {
      return(list(mat = mat, f = f))
    } else {
      return(f)
    }
  }
}
