# Test the get max theoretical fecundity fn
test_that("Confirm that unfished nums per recruit are lower than nums per recruit where E>0 (density dependence)", {
  f0 <- 1 / npr(S0 = 0.944, S1plus = 0.99, AgeMat = 17, nages = 10000, E = 0)$npr
  fmax <- getfecmax(lambdaMax = 1.04, S0 = 0.944, S1plus = 0.99, AgeMat = 17)
  expect_true(fmax > f0)
})
