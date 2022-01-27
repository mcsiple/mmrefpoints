#Test get_rf() function 

test_that("Confirm that get_rf() returns a single numeric value",
          {
            S0 = 0.944; S1plus = 0.99; nages = 25; AgeMat = 17; z = 2.39; lambdaMax = 1.04;
            NPROut <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = 0)
            N0 <- NPROut$npr # mature numbers per recruit
            
            # Get number of individuals per recruit in terms of individuals aged 1+ (P0.w)
            P0 <- NPROut$P1r # 1+ nums per recruit
            Fec0 <- 1.0 / N0
            FecMax <- getfecmax(S1plus = S1plus, S0 = S0, 
            AgeMat = AgeMat, lambdaMax = lambdaMax)
            A <- (FecMax - Fec0) / Fec0
            y <- get_rf(E_in = 0.01, S0 = S0, S1plus = S1plus,
                        nages = nages, AgeMat = AgeMat, z = z, A = A, P0 = P0, N0 = N0)
            expect_true(is.numeric(y))
          })
