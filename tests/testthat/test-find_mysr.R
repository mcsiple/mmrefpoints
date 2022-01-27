# Test the findmsyr() function

test_that("Test findmsyr", {
  # Set parameters
  lh.params <- list(
    S0.w = 0.5, S1plus.w = 0.944,
    nages.w = 25, K1plus.w = 9000,
    AgeMat.w = 18, z.w = 2.39,
    lambdaMax.w = 1.04
  )

  expect_error(find_msyr(E.start = 0.01, lh.params = list(
    S0 = 1.01, S1plus = S1plus.w, nages = nages.w,
    AgeMat = AgeMat.w, K1plus = 9000, z = z.w, lambdaMax = 1.09
  ), fmax = fmax))
})
