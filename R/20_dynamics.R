#' Generate one marine mammal population trajectory
#'
#' This function generates one trajectory for a marine mammal population, starting at a user-specified depletion level \code{InitDepl}.
#'
#' @details
#' The population model is a single-sex age-structured model in which the number of calves or pups born each year is density dependent, with the extent of density dependence a function of the number of mature adults \eqn{\tildeN}, the fecundity (pregnancy rate) at pre-exploitation equilibrium \eqn{f_0}, the maximum theoretical fecundity rate fmax, the degree of compensation \eqn{z}, and the abundance of individuals aged 1+ \eqn{N_{y+1}^{1+}} relative to carrying capacity \eqn{K^{1+}}. This function can be used alone but is intended to be used with \code{Projections()} to generate multiple simulations.
#'
#' @param S0 Calf or pup survival (a value between 0 and 1)
#' @param S1plus Survival for animals age 1 year and older (a value between 0 and 1)
#' @param K1plus The pre-exploitation population size of individuals aged 1 and older.  If this value is unavailable, it can be approximated by using the initial depletion and the estimate of current abundance
#' @param AgeMat Age at maturity in years (assumed to be age at first parturition - 1)
#' @param InitDepl Starting depletion level
#' @param ConstantCatch Total bycatch each year, expressed as a vector of length \code{nyears}
#' @param ConstantF vector (length = \code{nyears}) rate of bycatch each year
#' @param z The degree of compensation.  The default value is \code{z = 2.39}.
#' @param nyears Number of years to project
#' @param nages "Maximum" age, treated as the plus group age. The plus group age can be set equal to the age at maturity +2 years without losing accuracy
#' @param lambdaMax Maximum steady rate of increase (population growth rate)
#'
#' @return A list containing a matrix \code{N} of numbers at age (dimensions \code{nyears} (rows) x \code{nages} (columns)) and one vector \code{TotalPop} (a vector of length \code{nyears}), containing the number of age 1+ individuals in the population.

# Note, nages = PlusGroupAge, and PlusGroupAge can = AgeMat+2 without losing accuracy (per AEP 11/30/18)
#'
#' @examples
#' # Generate a time series of abundance for a bowhead whale
#' Dynamics(S0 = 0.944, S1plus = 0.99, K1plus = 9000, AgeMat = 17, InitDepl = 0.6, ConstantCatch = NA, ConstantF = rep(0.01, times = 100), z = 2.39, nyears = 100, nages = 25, lambdaMax = 1.04)
#' print(test)
#' @export
dynamics <- function(S0, S1plus, K1plus, AgeMat, InitDepl, ConstantCatch = NA, ConstantF = NA, z, nyears, nages, lambdaMax) {
  # Checks
  if (length(ConstantCatch) > 1 & length(ConstantF) > 1) {
    stop("Cannot have both constant F and constant catch- choose one and set the other to NA!")
  }

  if (InitDepl > 1) {
    InitDepl <- 1
  }
  nyrs <- nyears + 1
  AgePart <- AgeMat + 1 # Age at first parturition = age at maturity +1 (~gestation period)

  Neq <- Ninit <- vector(length = (nages + 1))
  N <- C <- matrix(0, nrow = nyrs, ncol = (nages + 1))

  Tot1P <- rep(0, length = nyrs)
  Nrep <- rep(0, length = nyrs) # number of reproductive individuals

  f0 <- (1 - S1plus) / (S0 * (S1plus)^(AgeMat - 1)) # analytical soln for f0
  fmax <- getfecmax(lambdaMax = lambdaMax, S1plus = S1plus, S0 = S0, AgeMat = AgeMat)

  # Equilibrium conditions (need outside of if() statement to get R0)
  Neq[1] <- 1 # Age 0
  Neq[2] <- S0 # Age 1
  for (a in 3:nages) {
    Neq[a] <- Neq[a - 1] * S1plus
  } # Age 2+
  Neq[nages + 1] <- (S0 * S1plus^(nages - 1)) / (1 - S1plus) # plus group
  R0 <- K1plus / sum(Neq[2:(nages + 1)]) # numerical soln


  # Initial conditions, equilibrium
  if (InitDepl == 1) { # pop starts at equilibrium
    N[1, ] <- Neq * R0
    Tot1P[1] <- K1plus
    Nrep[1] <- sum(N[1, AgePart:(nages + 1)])

    # Initial conditions, non-equilibrium
  } else { # pop starts at InitDepl*K
    NPROut <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = 0)
    N0 <- NPROut$npr # mature nums per recruit
    P0 <- NPROut$P1r # 1+ nums per recruit

    E <- get_f(
      f.start = 0.5,
      S0.w = S0,
      S1plus.w = S1plus,
      nages.w = nages,
      K1plus.w = K1plus,
      AgeMat.w = AgeMat,
      InitDepl.w = InitDepl,
      z.w = z,
      lambdaMax.w = lambdaMax,
      N0.w = N0,
      P0.w = P0
    )

    Ninit[1] <- 1 # N_exploited; Age 0
    Ninit[2] <- S0 # Age 1
    for (a in 3:nages) {
      Ninit[a] <- S0 * (S1plus * (1 - E))^(a - 2)
    }
    Ninit[nages + 1] <- (S0 * (S1plus * (1 - E))^(nages - 1)) / (1 - (S1plus * (1 - E)))

    #-----
    Fec0 <- 1.0 / N0
    A <- (fmax - Fec0) / Fec0

    RF <- get_rf(E = E, S0 = S0, S1plus = S1plus, nages = nages, K1plus = K1plus, AgeMat = AgeMat, z = z, A = A, P0 = P0, N0 = N0)
    InitNumsAtAge <- Ninit * RF # Initial nums at age
    PropsAtAge <- InitNumsAtAge / sum(InitNumsAtAge) # Proportions at age

    n0 <- PropsAtAge[1] / sum(PropsAtAge[-1]) # Proportion of the pop that is age 0
    N0.fished <- InitDepl * K1plus * n0 # Number of age 0 individuals @ the start

    N[1, ] <- N0.fished * Ninit

    Tot1P[1] <- sum(N[1, 2:(nages + 1)])
    Nrep[1] <- sum(N[1, AgePart:(nages + 1)])
  } # end initial conditions

  if (length(ConstantCatch) > 1) {
    for (Yr in 1:nyears) {
      MortE <- min(ConstantCatch[Yr] / Tot1P[Yr], 0.99) # bycatch mortality rate

      N[Yr + 1, 2] <- N[Yr, 1] * S0
      N[Yr + 1, 3:(nages + 1)] <- N[Yr, 2:nages] * (1 - MortE) * S1plus
      N[Yr + 1, (nages + 1)] <- (N[Yr, nages] + N[Yr, nages + 1]) * (1 - MortE) * S1plus
      Tot1P[Yr + 1] <- sum(N[Yr + 1, 2:(nages + 1)])
      Nrep[Yr + 1] <- sum(N[Yr + 1, AgePart:(nages + 1)])
      N[Yr + 1, 1] <- Nrep[Yr + 1] * (f0 + (fmax - f0) * (1 - (Tot1P[Yr + 1] / K1plus)^z))
    }
  } else {
    sel <- 1
    for (Yr in 1:nyears) {
      MortE <- ConstantF[Yr] * sel

      N[Yr + 1, 2] <- N[Yr, 1] * S0
      N[Yr + 1, 3:(nages + 1)] <- N[Yr, 2:nages] * (1 - MortE) * S1plus
      N[Yr + 1, (nages + 1)] <- (N[Yr, nages] + N[Yr, nages + 1]) * (1 - MortE) * S1plus
      Tot1P[Yr + 1] <- sum(N[Yr + 1, 2:(nages + 1)])
      Nrep[Yr + 1] <- sum(N[Yr + 1, AgePart:(nages + 1)])
      N[Yr + 1, 1] <- Nrep[Yr + 1] * (f0 + (fmax - f0) * (1 - (Tot1P[Yr + 1] / K1plus)^z)) # rec
    }
  }
  N <- N[-nyrs, ]
  Tot1P <- Tot1P[-nyrs]
  return(list(TotalPop = Tot1P, N = N))
}
