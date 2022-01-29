# Test multiplot_proj
test_that("multiplot_proj returns three-panel figure", {
  parms <- list(
    S0 = 0.944, S1plus = 0.99,
    K1plus = 9000,
    AgeMat = 18, nages = 20,
    z = 2.39, lambdaMax = 1.02
  )
  InitDepl.vec <- c(0.1, 0.5, 0.9)
  BycatchCV <- 0.2
  nyears <- 100

  high.list <- lapply(
    X = InitDepl.vec,
    function(x) {
      projections(
        NOut = 50,
        ConstantRateBycatch = list(Rate = 0.3, CV = BycatchCV),
        InitDepl = x,
        lh.params = parms,
        nyears = nyears,
        obs_CV = 0.2
      )
    }
  )

  med.list <- lapply(
    X = InitDepl.vec,
    function(x) {
      projections(
        NOut = 50,
        ConstantRateBycatch = list(Rate = 0.02, CV = BycatchCV),
        InitDepl = x,
        lh.params = parms,
        nyears = nyears,
        obs_CV = 0.2
      )
    }
  )
  low.list <- lapply(
    X = InitDepl.vec,
    function(x) {
      projections(
        NOut = 50,
        ConstantRateBycatch = list(Rate = 0.001, CV = BycatchCV),
        InitDepl = x,
        lh.params = parms,
        nyears = nyears,
        obs_CV = 0.2
      )
    }
  )
  p1 <- multiplot_proj(
    high.d1 = high.list[[1]], # d1 is the lowest depletion
    med.d1 = med.list[[1]],
    low.d1 = low.list[[1]],
    high.d2 = high.list[[2]],
    med.d2 = med.list[[2]],
    low.d2 = low.list[[2]],
    high.d3 = high.list[[3]],
    med.d3 = med.list[[3]],
    low.d3 = low.list[[3]],
    years.to.plot = nyears
  )

  expect_equal(3, length(ggplot2::ggplot_build(p1)$layout$layout$PANEL))
})
