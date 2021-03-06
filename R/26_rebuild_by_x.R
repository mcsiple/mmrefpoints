#' Calculate the bycatch rate needed to reach a performance goal
#'
#' @description takes a performance goal (what level you want to rebuild to) and a time window (how long you want that to take) and calculates what the bycatch rate needs to be
#'
#' @param needf.start starting guess for the bycatch mortality rate needed to recover the population
#' @param init.depl.w initial depletion (fraction of K)
#' @param goal.w what goal to rebuild to (MNPL is the simplest one)
#' @param desired.prob.w what probability you want (e.g., 75% probability that the population will rebuild to X in Y years)
#' @param when.w the year Y when you want rebuilding by
#' @param lh.params.w life history params (inputs for Projections())
#' @param fixed.cv.catch.w the CV of the bycatch rate - should be fixed
#'
#' @return The bycatch rate that will result in the specified rebuilding goal
#' @export
#'
#' @examples
#' rebuild_by_x(needf.start = 0.05, init.depl.w = 0.5, goal.w = 4500, desired.prob.w=0.8, when.w = 100, lh.params.w = list(S0 = 0.944, S1plus = 0.99, AgeMat = 17, nages = 19,  fmax = 0.29, z = 2.39, lambdaMax = 1.04, K1plus = 9000), fixed.cv.catch.w = 0)
rebuild_by_x <- function(needf.start, init.depl.w, goal.w, desired.prob.w, when.w, lh.params.w, fixed.cv.catch.w) {
  # Step 1: function to minimize
  mat <- matrix(0, 100, 2) # For storing outputs while uniroot is searching
  pointer <- 0
  set.seed(123)
  # plot(1:20,seq(0,0.3,length.out = 20),type='n',xlab="Iteration",ylab="Guess for best bycatch rate")
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
    # cat("pointer ", pointer, "prob", prob, "\n")
    pointer <<- pointer + 1 # A "naughty" way to get the fn to save guesses globally, so they can be plotted
    mat[pointer, ] <<- c(fishing.rate, prob)
    incProgress(1 / 15) # Shiny only line
    # points(pointer,fishing.rate,pch=19)
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
    # print(mat)
    # text(10,0.2,paste("Best estimate for \n max bycatch rate = ",round(f,digits=2)) )
    return(list(mat = mat, f = f))
  }
}
