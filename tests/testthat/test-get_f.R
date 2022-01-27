# Test get_f() function
test_that("get_f() returns a single numeric value",
          {
            S0.w = 0.5
            S1plus.w = 0.944
            nages.w = 25
            AgeMat.w = 18 
            InitDepl.w = 0.9
            z.w = 2.39
            lambdaMax.w = 1.04

            NPROut <- npr(S0 = S0.w, S1plus = S1plus.w, 
                          nages = nages.w, AgeMat = AgeMat.w, E = 0)
            N0 <- NPROut$npr # mature numbers per recruit
            P0 <- NPROut$P1r # 1+ nums per recruit
            
            # Get bycatch mortality rate for the initial depletion defined above
            x <- get_f(f.start = 0.1, 
                  S0.w = S0.w, S1plus.w = S1plus.w, nages.w = nages.w, AgeMat.w = AgeMat.w,
                  InitDepl.w = InitDepl.w, z.w = z.w, lambdaMax.w = lambdaMax.w, 
                  N0.w = N0, P0.w = P0, Check = FALSE)
            print(x)
            expect_true(is.numeric(x))
          })
