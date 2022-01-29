# Test dynamics()

test_that("Dynamics fn starts population at initial depletion * K1p",
          {
            K1p_in <- 5000
            InitDepl_in <- 0.6
            x <- dynamics(lh.params = list(S0 = 0.944, S1plus = 0.99, K1plus = K1p_in, 
                                           AgeMat = 17,nages = 25,z = 2.39, lambdaMax = 1.04),
                     InitDepl = InitDepl_in, ConstantCatch = NA, ConstantF = rep(0.01, times = 100), 
                     nyears = 100)
            expect_equal(K1p_in*InitDepl_in, x$TotalPop[1])
          })

test_that("Dynamics fn produces a list",
          {
            xx <- dynamics(lh.params = list(S0 = 0.944, S1plus = 0.99, K1plus = 9000, 
                                           AgeMat = 17,nages = 25,z = 2.39, lambdaMax = 1.04),
                          InitDepl = 0.6, ConstantCatch = NA, ConstantF = rep(0.01, times = 100), 
                          nyears = 100)
            expect_type(xx, "list")
          })

test_that("Dynamics fn produces non-NULL population and pop at age",
          {
            xx <- dynamics(lh.params = list(S0 = 0.93, S1plus = 0.99, K1plus = 4000, 
                                            AgeMat = 17,nages = 25,z = 2.39, lambdaMax = 1.04),
                           InitDepl = 0.6, ConstantCatch = NA, ConstantF = rep(0.01, times = 100), 
                           nyears = 100)
            expect_true(all(!is.null(xx$TotalPop)) & all(!is.null(xx$N)))
          }) 