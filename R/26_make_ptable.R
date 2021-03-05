#' Make a performance table
#'
#' @param traj.list A list of trajectories from \code{Projections()}
#' @param depletion A vector of starting depletions (abundance relative to carrying capacity)
#' @param lh.params A list of life history parameters
#' @param mnpl Max net productivity level (defined by user)
#'
#' @return
#' @export
#'
make_ptable <- function(traj.list, depletion, lh.params, mnpl) {
  # Each of these is one depletion level
  HiDepl1 <- unlist(lapply(X = traj.list, FUN = function(x) prob.rebuilt.goal(traj = x[[1]][["trajectories"]], goal = mnpl * lh.params$K1plus, rebuild.yr = 50)))
  # This is a list of the probability of recovery for each bycatch level, for the first depletion level
  MedDepl1 <- unlist(lapply(X = traj.list, FUN = function(x) prob.rebuilt.goal(traj = x[[2]][["trajectories"]], goal = mnpl * lh.params$K1plus, rebuild.yr = 50)))
  LowDepl1 <- unlist(lapply(X = traj.list, FUN = function(x) prob.rebuilt.goal(traj = x[[3]][["trajectories"]], goal = mnpl * lh.params$K1plus, rebuild.yr = 50)))

  # Same three depletion levels:
  HiDepl2 <- unlist(lapply(X = traj.list, FUN = function(x) prob.rebuilt.goal(traj = x[[1]][["trajectories"]], goal = mnpl * lh.params$K1plus, rebuild.yr = 100)))
  MedDepl2 <- unlist(lapply(X = traj.list, FUN = function(x) prob.rebuilt.goal(traj = x[[2]][["trajectories"]], goal = mnpl * lh.params$K1plus, rebuild.yr = 100)))
  LowDepl2 <- unlist(lapply(X = traj.list, FUN = function(x) prob.rebuilt.goal(traj = x[[3]][["trajectories"]], goal = mnpl * lh.params$K1plus, rebuild.yr = 100)))

  # make the table
  bycatch <- c("high", "med", "low", "zero")
  df <- expand_grid(bycatch, depletion)

  ptable <- expand_grid(depletion, bycatch) %>%
    add_column(
      prebuild50 = c(HiDepl1, MedDepl1, LowDepl1),
      prebuild100 = c(HiDepl2, MedDepl2, LowDepl2),
      abundrel10 = abund.rel.vec(traj.list = traj.list, K = lh.params$K, years.vec = 10),
      abundrel20 = abund.rel.vec(traj.list = traj.list, K = lh.params$K, years.vec = 20),
      abundrel50 = abund.rel.vec(traj.list = traj.list, K = lh.params$K, years.vec = 50)
    )

  return(ptable)
}
