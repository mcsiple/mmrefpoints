# test get_mnpl() function
test_that("MNPL should be higher when lambdaMax is higher", {
  aa <- get_mnpl(
    E.start = 0.001,
    lh.params = list(
      S0 = 0.944, S1plus = 0.99, AgeMat = 17, nages = 19,
      z = 2.39, lambdaMax = 1.04,
      K1plus = 9000
    )
  )
  bb <- get_mnpl(
    E.start = 0.001,
    lh.params = list(
      S0 = 0.944, S1plus = 0.99, AgeMat = 17, nages = 19,
      z = 2.39, lambdaMax = 1.12,
      K1plus = 9000
    )
  )
  expect_true(aa < bb)
})

test_that("MNPL and z should be convertible", {
  z_in <- 2.39
  cc <- get_mnpl(
    E.start = 0.001,
    lh.params = list(
      S0 = 0.944, S1plus = 0.99, AgeMat = 17, nages = 19,
      z = z_in, lambdaMax = 1.04,
      K1plus = 9000
    )
  )
  dd <- calc_z(MNPL_in = cc, lh.params_in = list(
    S0 = 0.944, S1plus = 0.99, AgeMat = 17, nages = 19,
    lambdaMax = 1.04,
    K1plus = 9000
  ))
  expect_equal(z_in, dd)
})
