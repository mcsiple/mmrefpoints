#' Calculate relative abundance (possibly depracated)
#' @param traj.list a list of simulation outputs (high.list,med.list,low.lis,zero.list) each of these lists is a list of three, one output at each starting depletion (high, med, low depletion)
#'
#' @param K carrying capacity
#' @param years.vec vector of years to check abundances at. If length = 1, the fn returns relative abundance at that year
abund_rel_vec <- function(traj.list, K = NA, years.vec = 10) {
  if (length(traj.list) != 4) {
    stop("Make sure all four outputs are here (high, med, low, zero bycatch)")
  }
  if (is.na(K)) {
    K <- traj.list[[1]][[1]]$params[1, "K1plus"]
  } # If K undefined, use K from outputs

  # Define the zero-bycatch outputs separately:
  traj.sublist.zero <- traj.list[[4]]

  # Define the list to store relative abundances
  relabund.list <- list()

  for (b in 1:4) {
    traj.sublist <- traj.list[[b]]
    relabund <- vector()
    for (depl in 1:3) {
      tsl <- traj.sublist[[depl]]
      zsl <- traj.sublist.zero[[depl]]$trajectories
      relabund[depl] <- abund.rel(traj = tsl$trajectories, zero.traj = zsl, K = K, years.vec = years.vec, fulldist = FALSE)
    }
    relabund.list[[b]] <- relabund # list order: high, med, low bycatch --> high, med, low depl
  }
  # currently sorted by bycatch, then depletion, need to switch:
  relabund.vec <- c(
    unlist(map(relabund.list, 1)), # high, med, low depletion
    unlist(map(relabund.list, 2)),
    unlist(map(relabund.list, 3)),
    unlist(map(relabund.list, 4))
  )
  # Now it's a sorted vector of high, med, low bycatch nested in high-med-low depletion

  return(relabund.vec)
}
