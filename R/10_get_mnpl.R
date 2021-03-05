#' Calculate MNPL
#'
#' @param E.start a starting guess for bycatch mortality rate that will result in MSYR (equivalent mathematically to FMSY for fisheries)
#' @param lh.params a list of life history parameters
#'
#' @export
get_mnpl <- function(E.start = 0.001, lh.params) {
  S0 <- lh.params$S0
  S1plus <- lh.params$S1plus
  nages <- lh.params$PlusGroupAge
  AgeMat <- lh.params$AgeMat
  lambdaMax <- lh.params$lambdaMax
  K1plus <- lh.params$K1plus
  z <- lh.params$z

  unex <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = 0)
  N0 <- unex$npr
  P0 <- unex$P1r
  fmax <- getfecmax(lambdaMax = lambdaMax, S1plus = S1plus, S0 = S0, AgeMat = AgeMat)
  Fec0 <- 1 / N0
  A <- (fmax - Fec0) / Fec0


  fmsy <- find_msyr(E.start = E.start, lh.params = lh.params, fmax = fmax, N0 = N0)


  Rf0 <- get_rf(E = 0, S0 = S0, S1plus = S1plus, nages = nages, K1plus = K1plus, AgeMat = AgeMat, z = z, A = A, P0 = P0, N0 = N0) # This is == 1
  Rfmsy <- get_rf(E = fmsy, S0 = S0, S1plus = S1plus, nages = nages, K1plus = K1plus, AgeMat = AgeMat, z = z, A = A, P0 = P0, N0 = N0)

  u1p <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = 0)$P1r * Rf0
  n1p <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = fmsy)$P1r * Rfmsy

  MNPL <- n1p / u1p # MNPL relative to unexploited
  # cat("z, LambdaMaxFMSY & MNPL",z,lambdaMax,fmsy, MNPL,"\n")
  return(MNPL)
}
