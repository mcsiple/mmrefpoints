# test get_diff function

test_that("get_diff returns errors when params are out of bounds",{
  
  # Set params - example species phocid seal
  S0.w = 0.802; S1plus.w = 0.920; nages.w = 8; AgeMat.w = 6 
  InitDepl.w = 0.9; z.w = 2.39; lambdaMax.w = 1.12
  
  # Calculate A
  NPROut <- npr(S0 = S0.w, S1plus = S1plus.w, nages = nages.w, AgeMat = AgeMat.w, E = 0)
  N0 <- NPROut$npr # mature numbers per recruit
  Fec0 <- 1.0 / N0
  fmax <- getfecmax(lambdaMax = lambdaMax.w, S0 = S0.w, S1plus = S1plus.w, AgeMat = AgeMat.w)
  A.w <- (fmax - Fec0) / Fec0
  
  expect_error(get_diff(
    logit.E = logit(0.01), S0 = S0.w, S1plus = 1.01, nages = nages.w, A = A.w, AgeMat = AgeMat.w, 
    z = 2.39
  ))
})
