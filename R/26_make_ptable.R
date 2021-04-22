#' Make a performance table. Trajectories must be based on 100-year simulations.
#'
#' @param traj.list A list of trajectories from \code{Projections()}
#' @param depletion A vector of starting depletions (abundance relative to carrying capacity)
#' @param lh.params A list of life history parameters
#' @param mnpl Max net productivity level (MNPL) defined by user. If MNPL is specified, that value is used. If the user does not specify MNPL, it is calculated from the life history parameters.
#'
#' @return A dataframe containing performance metrics. 
#' 
#' @examples
#' parms <- list(S0 = 0.944, S1plus = 0.99, K1plus = 9000, AgeMat = 18, 
#'               z = 2.39, lambdaMax = 1.02)
#' initdepl.vec <- c(0.2, 0.5, 0.9)
#' high.list.const <- lapply(
#'   X = initdepl.vec,
#'   function(x) {
#'     projections(
#'       NOut = 50,
#'       ConstantBycatch = list(Catch = 25, CV = 0.3),
#'       InitDepl = x,
#'       lh.params = parms,
#'       nyears = nyears,
#'       obs_CV = 0.1
#'     )
#'   }
#' )
#' med.list.const <- lapply(
#'   X = initdepl.vec,
#'   function(x) {
#'     projections(
#'       NOut = 50,
#'       ConstantBycatch = list(Catch = 12, CV = 0.3),
#'       InitDepl = x,
#'       lh.params = parms,
#'       nyears = nyears,
#'       obs_CV = 0.1
#'     )
#'   }
#' )
#' low.list.const <- lapply(
#'   X = initdepl.vec,
#'   function(x) {
#'     projections(
#'       NOut = 50,
#'       ConstantBycatch = list(Catch = 2, CV = 0.3),
#'       InitDepl = x,
#'       lh.params = parms,
#'       nyears = nyears,
#'       obs_CV = 0.1
#'     )
#'   }
#' )
#' zero.list.const <- lapply(
#'   X = initdepl.vec,
#'   function(x) {
#'     projections(
#'       NOut = 50,
#'       ConstantBycatch = list(Catch = 0, CV = 0),
#'       InitDepl = x,
#'       lh.params = parms,
#'       nyears = nyears,
#'       obs_CV = 0.1
#'     )
#'   }
#' )
#' traj.list <- list(
#'   high.list.const,
#'   med.list.const,
#'   low.list.const,
#'   zero.list.const
#' )
#' make_ptable(traj.list = traj.list, depletion = initdepl.vec, mnpl = 0.5)
#' 
#' @export
#'
make_ptable <- function(traj.list, depletion, mnpl = NA) {
  # check that there are enough years
  if(ncol(traj.list[[1]][[1]]$trajectories)<100){
    stop("Not enough years of projections. You need at least 100 years to calculate default performance measures")}
  
  # get parameters from trajectory list
  lh.params = as.list(traj.list[[1]][[1]]$params[1,])
  
  # If the user hasn't provided MNPL, calculate it.
  if(is.na(mnpl)){
    mnpl <- get_mnpl(E.start = 0.001, lh.params = lh.params)
  }
  
  
  # Each of these is one depletion level
  HiDepl1 <- unlist(lapply(X = traj.list, FUN = function(x) prob_rebuilt_goal(traj = x[[1]][["trajectories"]], goal = mnpl * lh.params$K1plus, rebuild.yr = 50)))
  # This is a list of the probability of recovery for each bycatch level, for the first depletion level
  MedDepl1 <- unlist(lapply(X = traj.list, FUN = function(x) prob_rebuilt_goal(traj = x[[2]][["trajectories"]], goal = mnpl * lh.params$K1plus, rebuild.yr = 50)))
  LowDepl1 <- unlist(lapply(X = traj.list, FUN = function(x) prob_rebuilt_goal(traj = x[[3]][["trajectories"]], goal = mnpl * lh.params$K1plus, rebuild.yr = 50)))

  # Same three depletion levels:
  HiDepl2 <- unlist(lapply(X = traj.list, FUN = function(x) prob_rebuilt_goal(traj = x[[1]][["trajectories"]], goal = mnpl * lh.params$K1plus, rebuild.yr = 100)))
  MedDepl2 <- unlist(lapply(X = traj.list, FUN = function(x) prob_rebuilt_goal(traj = x[[2]][["trajectories"]], goal = mnpl * lh.params$K1plus, rebuild.yr = 100)))
  LowDepl2 <- unlist(lapply(X = traj.list, FUN = function(x) prob_rebuilt_goal(traj = x[[3]][["trajectories"]], goal = mnpl * lh.params$K1plus, rebuild.yr = 100)))

  # make the table
  bycatch <- c("high", "med", "low", "zero")
#browser()
  ptable <- expand_grid(depletion, bycatch) %>%
    add_column(
      prebuild50 = c(HiDepl1, MedDepl1, LowDepl1),
      prebuild100 = c(HiDepl2, MedDepl2, LowDepl2),
      abundrel10 = abund_rel_vec(traj.list = traj.list, K = lh.params$K1plus, years.vec = 10),
      abundrel20 = abund_rel_vec(traj.list = traj.list, K = lh.params$K1plus, years.vec = 20),
      abundrel50 = abund_rel_vec(traj.list = traj.list, K = lh.params$K1plus, years.vec = 50)
    )

  return(ptable)
}
