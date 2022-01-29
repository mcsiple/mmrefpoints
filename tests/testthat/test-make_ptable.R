# Test the make_ptable function

test_that("make_ptable produces a dataframe", {
  parms <- list(
    S0 = 0.944, S1plus = 0.99, K1plus = 9000, AgeMat = 18, nages = 20,
    z = 2.39, lambdaMax = 1.02
  )
  initdepl.vec <- c(0.2, 0.5, 0.9)
  nyears <- 100
  high.list.const <- lapply(
    X = initdepl.vec,
    function(x) {
      projections(
        NOut = 50,
        ConstantBycatch = list(Catch = 25, CV = 0.3),
        InitDepl = x,
        lh.params = parms,
        nyears = nyears,
        obs_CV = 0.1
      )
    }
  )
  med.list.const <- lapply(
    X = initdepl.vec,
    function(x) {
      projections(
        NOut = 50,
        ConstantBycatch = list(Catch = 12, CV = 0.3),
        InitDepl = x,
        lh.params = parms,
        nyears = nyears,
        obs_CV = 0.1
      )
    }
  )
  low.list.const <- lapply(
    X = initdepl.vec,
    function(x) {
      projections(
        NOut = 50,
        ConstantBycatch = list(Catch = 2, CV = 0.3),
        InitDepl = x,
        lh.params = parms,
        nyears = nyears,
        obs_CV = 0.1
      )
    }
  )
  zero.list.const <- lapply(
    X = initdepl.vec,
    function(x) {
      projections(
        NOut = 50,
        ConstantBycatch = list(Catch = 0, CV = 0),
        InitDepl = x,
        lh.params = parms,
        nyears = nyears,
        obs_CV = 0.1
      )
    }
  )
  traj.list <- list(
    high.list.const,
    med.list.const,
    low.list.const,
    zero.list.const
  )
  xx <- make_ptable(traj.list = traj.list, depletion = initdepl.vec, mnpl = 0.5)
  expect_s3_class(xx, "tbl_df")
})

test_that("Probabilities and relative abundances to not go above 1", {
  parms <- list(
    S0 = 0.944, S1plus = 0.99, K1plus = 9000, AgeMat = 18, nages = 20,
    z = 2.39, lambdaMax = 1.02
  )
  initdepl.vec <- c(0.2, 0.5, 0.9)
  nyears <- 100
  high.list.const <- lapply(
    X = initdepl.vec,
    function(x) {
      projections(
        NOut = 50,
        ConstantBycatch = list(Catch = 25, CV = 0.3),
        InitDepl = x,
        lh.params = parms,
        nyears = nyears,
        obs_CV = 0.1
      )
    }
  )
  med.list.const <- lapply(
    X = initdepl.vec,
    function(x) {
      projections(
        NOut = 50,
        ConstantBycatch = list(Catch = 12, CV = 0.3),
        InitDepl = x,
        lh.params = parms,
        nyears = nyears,
        obs_CV = 0.1
      )
    }
  )
  low.list.const <- lapply(
    X = initdepl.vec,
    function(x) {
      projections(
        NOut = 50,
        ConstantBycatch = list(Catch = 2, CV = 0.3),
        InitDepl = x,
        lh.params = parms,
        nyears = nyears,
        obs_CV = 0.1
      )
    }
  )
  zero.list.const <- lapply(
    X = initdepl.vec,
    function(x) {
      projections(
        NOut = 50,
        ConstantBycatch = list(Catch = 0, CV = 0),
        InitDepl = x,
        lh.params = parms,
        nyears = nyears,
        obs_CV = 0.1
      )
    }
  )
  traj.list <- list(
    high.list.const,
    med.list.const,
    low.list.const,
    zero.list.const
  )
  xx <- make_ptable(traj.list = traj.list, depletion = initdepl.vec, mnpl = 0.5)

  expect_false(any(xx[, c("prebuild50", "prebuild100", "abundrel10", "abundrel20", "abundrel50")] > 1))
})
