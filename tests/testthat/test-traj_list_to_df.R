test_that("check that traj_list_to_df returns the right number of dims", {
  parms <- list(
    S0 = 0.944, S1plus = 0.99, K1plus = 9000, AgeMat = 18,
    nages = 25, z = 2.39, lambdaMax = 1.02
  )
  nyears <- 50
  initdepl.vec <- c(0.2, 0.5, 0.9)
  nout <- 50
  high.list.const <- lapply(
    X = initdepl.vec,
    function(x) {
      projections(
        NOut = nout,
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
        NOut = nout,
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
        NOut = nout,
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
        NOut = nout,
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

  test_df <- traj_list_to_df(traj.list)

  expect_equal(nrow(test_df), length(initdepl.vec) * length(traj.list) * nout)
})
