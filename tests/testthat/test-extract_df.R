test_that("extract_df() returns a dataframe w the correct number of sims",
          {
            nyrs_test <- 100
            xx <- projections(
              NOut = 3, ConstantRateBycatch = list(Rate = 0.01, CV = 0.3),
              InitDepl = 0.8,
              lh.params = list(
                S0 = 0.944, S1plus = 0.99,
                K1plus = 9000, AgeMat = 18, nages = 20, z = 2.39, lambdaMax = 1.02
              ),
              nyears = nyrs_test, obs_CV = 0
            )
            yy <- extract_df(xx)
            expect_true(colnames(yy)[nyrs_test+1]=="sim")
          })

test_that("extract_df() has all needed columns from the projections",
          {
          
            xx <- projections(
              NOut = 3, ConstantRateBycatch = list(Rate = 0.01, CV = 0.3),
              InitDepl = 0.8,
              lh.params = list(
                S0 = 0.944, S1plus = 0.99,
                K1plus = 9000, AgeMat = 18, nages = 20, z = 2.39, lambdaMax = 1.02
              ),
              nyears = 50, obs_CV = 0
            )
            yy <- extract_df(xx)
            expect_true("sim" %in% colnames(yy) & 
                          "Bycatch_Catch" %in% colnames(yy) &
                          "Bycatch_CV" %in% colnames(yy) &
                          "Bycatch_Rate" %in% colnames(yy) &
                          "Bycatch_Rate_CV" %in% colnames(yy) &
                          "InitDepl" %in% colnames(yy))
          })
