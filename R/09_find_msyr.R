#' calculate MSYR
#'
#' @description one of the functions needed for getting MNPL. MSYR is the same as FMSY.
#'
#' @param E.start a starting guess for bycatch mortality rate that will result in FMSY (numeric value)
#' @param lh.params a list of life history parameters (S0, S1plus, nages, AgeMat, lmabdaMax, K1plus, and z)
#' @param unpr unfished numbers per recruit (num)
#' @param fmax Max theoretical fecundity (num)
#'
#' @export
find_msyr <- function(E.start, lh.params, fmax, N0) {
  S0.w <- lh.params$S0
  S1plus.w <- lh.params$S1plus
  nages.w <- lh.params$PlusGroupAge
  AgeMat.w <- lh.params$AgeMat
  lambdaMax.w <- lh.params$lambdaMax
  K1plus.w <- lh.params$K1plus
  z.w <- lh.params$z

  unex <- npr(S0 = S0.w, S1plus = S1plus.w, nages = nages.w, AgeMat = AgeMat.w, E = 0)
  # N0 <- unex$npr
  P0 <- unex$P1r

  Fec0 <- 1.0 / N0
  A <- (fmax - Fec0) / Fec0

  search.limit <- get_f(
    f.start = 0.005,
    S0.w = S0.w,
    S1plus.w = S1plus.w,
    nages.w = nages.w,
    K1plus.w = K1plus.w,
    AgeMat.w = AgeMat.w,
    InitDepl.w = 0.000001, # 0 causes strange behavior
    z.w = z.w,
    lambdaMax.w = lambdaMax.w,
    N0.w = N0,
    P0.w = P0
  ) * .95
  # cat("Search limit",search.limit,"\n")

  lims <- logit(c(0.00001, search.limit))
  logit.E <- logit(E.start)

  zero.cross <- uniroot(f = getdC, interval = lims, tol = 1e-7, S0 = S0.w, S1plus = S1plus.w, nages = nages.w, AgeMat = AgeMat.w, lambdaMax = lambdaMax.w, K1plus = K1plus.w, z = z.w)
  fmsy <- inv_logit(zero.cross$root)

  MSY <- ce(
    S0 = S0.w,
    S1plus = S1plus.w,
    nages = nages.w,
    K1plus = K1plus.w,
    AgeMat = AgeMat.w,
    z = z.w,
    lambdaMax = lambdaMax.w,
    E = fmsy,
    A = A, P0 = P0, N0 = N0
  )
  if (MSY == 0) cat("Warning FMSY is not correct", "\n")
  # cat("FMSY Final",fmsy,zero.cross$f.root,MSY,"\n")
  return(fmsy)
}

# findMSYR(E.start = 0.005,
#          lh.params = lh.params1,
#          fmax = getFecmax2(lambdaMax = lh.params1$lambdaMax,S1plus = lh.params1$S1plus,S0 = lh.params1$S0,AgeMat = lh.params1$AgeMat),N0 = NPR(S0 = lh.params1$S0,S1plus = lh.params1$S1plus,nages = lh.params1$nages,AgeMat = lh.params1$AgeMat,f = 0)$npr)
