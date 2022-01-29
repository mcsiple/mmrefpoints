# plot_proj test
test_that("plot_proj() errors when it tries to plot more years than in projections", {
  parms <- list(
    S0 = 0.77, S1plus = 0.88,
    K1plus = 9000,
    AgeMat = 3, nages = 10,
    z = 2.39, lambdaMax = 1.12
  )
  initdepl <- 0.5
  high.simple <- projections(
    NOut = 50,
    ConstantBycatch = list(
      Catch = 100,
      CV = 0.3
    ),
    InitDepl = initdepl,
    lh.params = parms,
    nyears = 100
  )
  med.simple <- projections(
    NOut = 50,
    ConstantBycatch = list(
      Catch = 50,
      CV = 0.3
    ),
    InitDepl = initdepl,
    lh.params = parms,
    nyears = 100
  )

  low.simple <- projections(
    NOut = 50,
    ConstantBycatch = list(
      Catch = 10,
      CV = 0.3
    ),
    InitDepl = initdepl,
    lh.params = parms,
    nyears = 100
  )

  expect_error(plot_proj(
    high = high.simple,
    med = med.simple,
    low = low.simple,
    years.plot = 101,
    ylims = c(0, parms$K1plus), InitDepl = initdepl,
    K1plus = parms$K1plus
  ))
})

test_that("plot_proj() errors when it tries to plot more years than in projections", {
  parms <- list(
    S0 = 0.77, S1plus = 0.88,
    K1plus = 9000,
    AgeMat = 3, nages = 10,
    z = 2.39, lambdaMax = 1.12
  )
  initdepl <- 0.5
  high.simple <- projections(
    NOut = 50,
    ConstantBycatch = list(
      Catch = 100,
      CV = 0.3
    ),
    InitDepl = initdepl,
    lh.params = parms,
    nyears = 100
  )
  med.simple <- projections(
    NOut = 50,
    ConstantBycatch = list(
      Catch = 50,
      CV = 0.3
    ),
    InitDepl = initdepl,
    lh.params = parms,
    nyears = 100
  )

  low.simple <- projections(
    NOut = 50,
    ConstantBycatch = list(
      Catch = 10,
      CV = 0.3
    ),
    InitDepl = initdepl,
    lh.params = parms,
    nyears = 100
  )

  p1 <- plot_proj(
    high = high.simple,
    med = med.simple,
    low = low.simple,
    years.plot = 100,
    ylims = c(0, parms$K1plus), InitDepl = initdepl,
    K1plus = parms$K1plus
  )

  expect_true(ggplot2::is.ggplot(p1))
})
