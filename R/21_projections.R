#' Run simulations of a marine mammal population with bycatch mortality
#'
#' @description - generates several projections, stochasticity is in the number of catches from year to year
#' @param NOut - number of simulations
#' @param ConstantBycatch Mean and CV of number of animals killed as bycatch per year
#' @param ConstantRateBycatch Mean and CV of bycatch rate
#' @param InitDepl initial depletion. If obs_CV>0, this is the mean depletion.
#' @param lh.params - life history parameters as a list. The list must include S0, S1plus, K1plus, AgeMat, nages, z, and lambdaMax
#' @param nyears number of years to do projections
#' @param obs_CV observation CV. Default to 1 for simple projections
#' @return list of outputs from simulations: \code{params} contains parameter values for each trajectory; \code{trajectories} matrix contains simulation outputs; \code{fishing.rates} contain the bycatch rates for each year in each simulation; \code{InitDepl} returns the initial depletion for the projections; \code{ConstantBycatch} provides Catch (total individuals killed in bycatch events per year) and CV of Catch (if the user has specified bycatch as a constant number); \code{ConstantRateBycatch} contains Bycatch Rate (additional mortality from bycatch each year)  and CV of ByCatch rate. Other parameters are the same as in the \code{dynamics()} function.
#'
#' @examples 
#' projections(NOut = 3, ConstantRateBycatch = list(Rate = 0.01, CV = 0.3),
#' InitDepl = 0.8,
#' lh.params = list(S0=0.944,S1plus=0.99,
#' K1plus=9000,AgeMat=18,z= 2.39,lambdaMax=1.02),
#' nyears = 50, obs_CV = 0)
#' 
#' @export
projections <- function(NOut,
                        ConstantBycatch = list(Catch = NA, CV = NA),
                        ConstantRateBycatch = list(Rate = NA, CV = NA), # need to generate values from popbio{}
                        InitDepl,
                        lh.params,
                        nyears,
                        obs_CV = 0) {
  # Bowhead default for life history: lh.params = list(S0=0.944,S1plus=0.99,fmax=0.29,K1plus=9000,AgeMat=18,Plus Group Age=25,z= 2.39,lambdaMax=1.02)

  trajectories <- rep(0, times = nyears)
  fishing.rates <- rep(0, times = nyears)
  params <- 0 # S0, S1plus, fmax, K1plus, z # also need: nyears = 50, nages = 15
  NTries <- 0


  # Life history params
  S0 <- lh.params$S0
  S1plus <- lh.params$S1plus
  K1plus <- lh.params$K1plus
  AgeMat <- lh.params$AgeMat
  nages <- lh.params$nages
  z <- lh.params$z
  lambdaMax <- lh.params$lambdaMax

  # Checks for parameter values
  if(S0 >= 1){warning("Calf/pup survival must be between 0 and 1")}
  if(S1plus >= 1){warning("Adult survival must be between 0 and 1")}
  if(AgeMat > nages){warning("Age at maturity must be less than plus group age")}
  if(!is.na(ConstantBycatch$Catch) & !is.na(ConstantRateBycatch$Rate)){
    warning("You cannot provide both bycatch as a whole number and as a rate. 
         Please specify either ConstantBycatch or ConstantRateBycatch.")}
  if(lambdaMax < 1){warning("LambdaMax should be greater than 1. 
                         Typical values are lambdaMax = 1.04 for cetaceans and lambdaMax = 1.12 for pinnipeds.")}
  
  set.seed(123)

  # Observation error
  MeanObs <- InitDepl * K1plus
  sdlog.obs <- sqrt(log(obs_CV^2 + 1))
  meanlog <- log(MeanObs) - (sdlog.obs^2) / 2


  while (NTries < NOut) {
    # INDIVIDUALS PER YEAR
    if (!is.na(ConstantBycatch$Catch)) {
      # Observation error
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
      )$TotalPop # 1+ component of population

      param.vec <- unlist(lh.params)
      if (NTries < NOut) {
        params <- rbind(params, param.vec)
        trajectories <- rbind(trajectories, traj)
        NTries <- NTries + 1
      }
    }
  } # end of sims
  
  if(!is.null(nrow(fishing.rates))){
    fishing.rates <- fishing.rates[-1, ]} # trim top row of zeroes for cases where it's entered as a rate instead of a absolute # per year
  trajectories <- trajectories[-1, ]
  
  return(list(
    params = params[-1, ],
    trajectories = trajectories,
    fishing.rates = fishing.rates,
    InitDepl = InitDepl,
    ConstantBycatch = unlist(ConstantBycatch),
    ConstantRateBycatch = unlist(ConstantRateBycatch)
  ))
} # end Projections() function
