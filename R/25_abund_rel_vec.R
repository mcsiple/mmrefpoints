#' Calculate relative abundance
#'
#' @description This function returns a vector of relative abundances based on a list of projection outputs. It is primarily for building the performance table.
#' @param traj.list a list of simulation outputs (high.list,med.list,low.lis,zero.list) each of these lists is a list of three, one output at each starting depletion (high, med, low depletion)
#' @param K carrying capacity
#' @param years.vec vector of years to check abundances at. If length = 1, the fn returns relative abundance at that year
#' @return A vector of relative abundances sorted by bycatch (high, med, low, zero) within depletion level (low, med, high). 
#' @examples 
#' parms <- list(S0 = 0.944, S1plus = 0.99, K1plus = 9000, AgeMat = 18, 
#'               nages = 25, z = 2.39, lambdaMax = 1.02)
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
#' 
#' abund_rel_vec(traj.list = traj.list, K = parms$K1plus, years.vec = 10)
#' 
#' @export
#' 
abund_rel_vec <- function(traj.list, K = NA, years.vec = 10) {
  # Check dimensions
  if (length(traj.list) != 4) {
    stop("Make sure all four outputs are here (high, med, low, zero bycatch)")
  }
  proj.length <- length(traj.list[[1]][[1]][["fishing.rates"]])
  if(years.vec > proj.length){
    x <- paste("Years to evaluate relative abundance for exceeds the number of years in the projection. Try again with years.vec <=", proj.length, sep = " ")
    stop(x)}
  
  # If K undefined, use K from outputs
  if (is.na(K)) {
    K <- traj.list[[1]][[1]][["params"]][1, "K1plus"]
  } 
  
  
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
      relabund[depl] <- abund_rel(traj = tsl$trajectories, 
                                  zero.traj = zsl, K = K, 
                                  years.vec = years.vec, 
                                  fulldist = FALSE)
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
  # Now it's a sorted vector of high, med, low bycatch nested in low-med-high  depletion

  return(relabund.vec)
}
