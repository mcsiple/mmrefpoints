#' Get bycatch mortality rate given depletion
#'
#' This function solves for the bycatch mortality rate \eqn{E} that gives a pre-specified depletion level \code{InitDepl.w}. It is used within the \code{projections()} function to calculate the stable age distribution at which to start the projections.
#'
#' @param f.start an initial guess for the bycatch mortality rate E. The default value is E = 0.5
#' @param S0.w Calf/pup survival, a numeric value between 0 and 1. (Note: the 'w' suffix indicates that \eqn{z} is in the wrapper function, and is used inside the function by \code{optim})
#' @param S1plus.w  survival rate for animals age 1 year and older, a numeric value between 0 and 1
#' @param AgeMat.w Age at maturity in years (assumed to be age at first parturition - 1)
#' @param nages.w "maximum" age, treated as the plus group age. The plus group age can be set equal to the age at maturity +2 years without losing accuracy.
#' @param InitDepl.w The depletion level to solve for as a proportion of carrying capacity; a numeric value between 0 and 1. This is equivalent to 'starting depletion'.
#' @param z.w The degree of compensation. The default value is \code{z = 2.39}.
#' @param lambdaMax.w The maximum intrinsic growth rate
#' @param P0.w Number of individuals per recruit in terms of individuals aged 1+
#' @param Check logical; if \code{TRUE}, prints the value to be minimized (should be zero)
#' @param N0.w unfished numbers per recruit in terms of mature individuals individuals
#'
#' @return The bycatch rate that would lead to a depletion level of \code{InitDepl.w} -- a single value.
#'
#' @examples
#' # Set parameters
#' S0.w = 0.5; S1plus.w = 0.944; nages.w = 25; AgeMat.w = 18 
#' InitDepl.w = 0.9; z.w = 2.39; lambdaMax.w = 1.04
#' # Get number of individuals per recruit in terms of mature individuals (\eqn{N0.w})
#' NPROut <- npr(S0 = S0.w, S1plus = S1plus.w, nages = nages.w, AgeMat = AgeMat.w, E = 0)
#' N0 <- NPROut$npr # mature numbers per recruit
#' # Get number of individuals per recruit in terms of individuals aged 1+ (\eqn{P0.w})
#' P0 <- NPROut$P1r # 1+ nums per recruit
#'
#' # Get bycatch mortality rate for the initial depletion defined above
#' get_f(f.start = 0.5, 
#' S0.w = S0.w, S1plus.w = S1plus.w, nages.w = nages.w, AgeMat.w = AgeMat.w,
#' InitDepl.w = InitDepl.w, z.w = z.w, lambdaMax.w = lambdaMax.w, 
#' N0.w = N0, P0.w = P0,Check=T)
#' @export
get_f <- function(f.start = NA, S0.w = NA, S1plus.w = NA, 
                  nages.w = NA, AgeMat.w = NA, InitDepl.w = NA, 
                  z.w = NA, lambdaMax.w = NA, 
                  N0.w = NA, P0.w = NA, 
                  Check = F) {
  # checks
  if(AgeMat.w > nages.w){warning("Age at maturity cannot be larger than plus group age. Change AgeMat or nages.")}
  if(S0.w < 0 | S0.w >= 1){stop("Calf/pup survival must be between 0 and 1")}
  if(S1plus.w < 0 | S1plus.w >= 1){stop("Adult survival must be between 0 and 1")}
  if(f.start < 0 | f.start >= 1){stop("f.start must be between 0 and 1")}
  if(lambdaMax.w < 1){stop("lambdaMax.w must be greater than 1")}
  
  # fecundity at unfished equilibrium
  Fec0 <- 1.0 / N0.w
  FecMax <- getfecmax(S1plus = S1plus.w, S0 = S0.w, AgeMat = AgeMat.w, lambdaMax = lambdaMax.w)
  A <- (FecMax - Fec0) / Fec0 # Pella-Tomlinson resilience parameter, from Punt 1999 (Annex R) - Note: this is the simpler of two ways to get A
  
  # set limit for uniroot. Don't search too far in the positive direction
  search.limit <- (1 - (1 / lambdaMax.w)) * 2.5
  
  logit.start <- logit(f.start)
  to.minimize <- function(lf = logit.start,
                          S0 = S0.w, S1plus = S1plus.w,
                          nages = nages.w,
                          AgeMat = AgeMat.w, InitDepl = InitDepl.w,
                          z = z.w, lambdaMax = lambdaMax.w,
                          P0 = P0.w, # 1+ adults per recruit @E=0
                          N0 = N0.w # mature adults per recruit
  ) {
    f <- inv_logit(lf) # f ~= 0.01
    
    # Get numbers per recruit at f=f
    NPR <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = f) 
    PE <- NPR$P1r
    
    # Compute unfished recruitment (set to 1) and fished recruitment
    R0 <- 1 
    R.F <- get_rf(E_in = f, S0 = S0,S1plus = S1plus,nages = nages, AgeMat = AgeMat, z = z, A = A, P0 = P0, N0 = N0)
    
    # Calculate the depletion in terms of nature females
    Pt1 <- R.F * PE / (R0 * P0)
    
    # variable to solve for
    diff <- Pt1 - InitDepl
    return(diff)
  }
  
  zero.cross <- stats::uniroot(
    f = to.minimize,
    interval = logit(c(0.00001, search.limit)),
    tol = 1e-7
  )
  f <- inv_logit(zero.cross$root)
  
  # Check to make sure value of to-minimize is zero
  if (Check==T)
  {  
    to.min.val <- to.minimize(lf = logit(f))
    cat("   val of to.minimize (should be 0)",to.min.val, "/n")
  }
  
  return(f)
}
