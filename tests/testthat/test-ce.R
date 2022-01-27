# Test ce() function
test_that("ce errors when calf survival out of bounds", {
  # example species: bottlenose dolphin
  S0.w <- 0.865
  S1plus.w <- 0.951
  nages.w <- 10
  AgeMat.w <- 6
  # Get number of individuals per recruit in terms of mature individuals (N0.w)
  NPROut <- npr(S0 = S0.w, S1plus = S1plus.w, nages = nages.w, AgeMat = AgeMat.w, E = 0)

  N0 <- NPROut$npr # mature numbers per recruit
  # Get number of individuals per recruit in terms of individuals aged 1+ (P0.w)
  P0 <- NPROut$P1r # 1+ nums per recruit

  expect_error(ce(
    S0 = 1.1, S1plus = S1plus.w,
    nages = nages.w,
    AgeMat = AgeMat.w,
    E = 0.01, z = 2.39, A = 2, N0 = N0, P0 = P0
  ))
})

test_that("ce (normalized sustainable yield) is higher when S1+ is higher", {
  # example species: bottlenose dolphin
  S0.w <- 0.865
  S1plus.w <- 0.951
  nages.w <- 10
  AgeMat.w <- 6

  NPROut <- npr(S0 = S0.w, S1plus = S1plus.w, nages = nages.w, AgeMat = AgeMat.w, E = 0)

  N0 <- NPROut$npr # mature numbers per recruit
  P0 <- NPROut$P1r # 1+ nums per recruit

  x <- ce(
    S0 = S0.w, S1plus = 0.81,
    nages = nages.w,
    AgeMat = AgeMat.w,
    E = 0.01, z = 2.39, A = 2, N0 = N0, P0 = P0
  )

  y <- ce(
    S0 = S0.w, S1plus = 0.99,
    nages = nages.w,
    AgeMat = AgeMat.w,
    E = 0.01, z = 2.39, A = 2, N0 = N0, P0 = P0
  )

  expect_true(y > x)
})
