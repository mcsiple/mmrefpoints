# Test the plot_yield_curve function
test_that("Yield curve function returns a ggplot object",
          {
            p1 <- plot_yield_curve(
                  lh.params = list(
                    S0 = 0.944, S1plus = 0.99, AgeMat = 17,
                    nages = 19, lambdaMax = 1.02, K1plus = 9000
                  ),
                  MNPL_in = 0.5, z = NA, lang = "en"
                )
            expect_true(ggplot2::is.ggplot(p1))
          })

test_that("Yield curve function returns a ggplot object when z is provided",
          {
            p2 <- plot_yield_curve(
              lh.params = list(
                S0 = 0.944, S1plus = 0.99, AgeMat = 17,
                nages = 19, lambdaMax = 1.02, K1plus = 9000
              ),
              MNPL_in = 0.5, z = 2.30, lang = "en"
            )
            expect_true(ggplot2::is.ggplot(p2))
          })
