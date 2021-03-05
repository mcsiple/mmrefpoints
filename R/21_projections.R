#' Run multiple simulations of a marine mammal population with bycatch mortality
#'
#' @description - generates several projections, stochasticity is in the number of catches from year to year
#' @param lh.params - life history parameters (as a list)
#' @param NOut - number of simulations
#' @param ConstantBycatch Mean and CV of number of animals caught as bycatch each year
#' @param ConstantRateBycatch Mean and CV of bycatch rate
#' @param InitDepl initial depletion. If obs_CV>0, this is the mean depletion
#' @param lh.params a list of life history parameters
#' @param nyears number of years to do projections
#' @param obs_CV observation CV. Default to 1 for simple projections
#' @return list of outputs from simulations: 'trajectories' matrix has simulations
#' other params are the same as in the Dynamics() function
#'
#' @export
projections <- function(NOut,
                        ConstantBycatch = list(Catch = NA, CV = NA),
                        ConstantRateBycatch = list(Rate = NA, CV = NA), # need to generate values from popbio{}
                        InitDepl,
                        lh.params,
                        nyears,
                        obs_CV = 0) {
  # Bowhead default for life history: lh.params = list(S0=0.944,S1plus=0.99,fmax=0.29,K1plus=9000,AgeMat=18,PlusGroupAge=25,z= 2.39,lambdaMax=1.02)

  trajectories <- rep(0, times = nyears)
  fishing.rates <- rep(0, times = nyears)
  params <- 0 # S0, S1plus, fmax, K1plus, z # also need: nyears = 50, nages = 15
  NTries <- 0


  # Life history params
  S0 <- lh.params$S0
  S1plus <- lh.params$S1plus
  fmax <- lh.params$fmax
  K1plus <- lh.params$K1plus
  AgeMat <- lh.params$AgeMat
  nages <- lh.params$PlusGroupAge
  z <- lh.params$z
  lambdaMax <- lh.params$lambdaMax

  set.seed(123)

  # Observation error
  MeanObs <- InitDepl * K1plus
  sdlog.obs <- sqrt(log(obs_CV^2 + 1))
  meanlog <- log(MeanObs) - (sdlog.obs^2) / 2


  while (NTries < NOut) {
    # INDIVIDUALS PER YEAR
    if (!is.na(ConstantBycatch$Catch)) {
      # Observation error
      # InitVal <- exp(rnorm(1,meanlog,sdlog.obs)) # "true" abundance is distributed around the mean that the user provides
      InitVal <- rlnorm(1, meanlog, sdlog.obs)
      sdlog.catches <- sqrt(log(ConstantBycatch$CV^2 + 1)) # should be ~cv when sd is low (<0.5)
      catch.vec <- rlnorm(n = nyears, meanlog = log(ConstantBycatch$Catch), sdlog = sdlog.catches)
      traj <- dynamics(
        S0 = S0, S1plus = S1plus, K1plus = K1plus, AgeMat = AgeMat, InitDepl = InitVal / K1plus,
        ConstantCatch = catch.vec,
        z = z, nyears = nyears, nages = nages, lambdaMax = lambdaMax
      )$TotalPop # TotalPop is 1+ component
      param.vec <- unlist(lh.params)
      if (NTries < NOut) {
        params <- rbind(params, param.vec)
        trajectories <- rbind(trajectories, traj)
        NTries <- NTries + 1
      }
    }
    # PROPORTION
    if (!is.na(ConstantRateBycatch$Rate)) {

      # InitVal <- exp(rnorm(1,meanlog,sdlog.obs)) # "true" abundance is distributed around the mean that the user provides
      InitVal <- rlnorm(1, meanlog, sdlog.obs)
      CV <- ifelse(ConstantRateBycatch$Rate == 0, 0, ConstantRateBycatch$CV) # betaval() doesn't work if mn=0 and sd>0
      # Generate random catch rates around mean
      mn <- ConstantRateBycatch$Rate
      stdev <- CV * mn
      if (stdev != 0 && (stdev)^2 >= ((1 - mn) * mn)) {
        stdev <- 0.00000001
      }

      f.vec <- vector()
      for (y in 1:nyears) {
        f.vec[y] <- popbio::betaval(mn = mn, sdev = stdev)
      }
      fishing.rates <- rbind(fishing.rates, f.vec)

      traj <- dynamics(
        S0 = S0, S1plus = S1plus, K1plus = K1plus, AgeMat = AgeMat, InitDepl = InitVal / K1plus,
        ConstantF = f.vec,
        z = z, nyears = nyears, nages = nages, lambdaMax = lambdaMax
      )$TotalPop # TotalPop is 1+ component

      param.vec <- unlist(lh.params)
      if (NTries < NOut) {
        params <- rbind(params, param.vec)
        trajectories <- rbind(trajectories, traj)
        NTries <- NTries + 1
      }
    }
  } # end of sims
  trajectories <- trajectories[-1, ] # trim top row of zeroes
  return(list(
    params = params[-1, ],
    trajectories = trajectories,
    fishing.rates = fishing.rates,
    InitDepl = InitDepl,
    ConstantBycatch = unlist(ConstantBycatch),
    ConstantRateBycatch = unlist(ConstantRateBycatch)
  ))
} # end Projections() function
