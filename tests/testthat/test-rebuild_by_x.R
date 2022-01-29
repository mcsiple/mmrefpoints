# test rebuild by x
test_that("Check that errors produced when depletion outside of bounds",
          {
            expect_error(rebuild_by_x(
              needf.start = 0.001,
              init.depl.w = 1.01, goal.w = 4500,
              desired.prob.w = 0.8, when.w = 100,
              lh.params.w = list(
                S0 = 0.944, S1plus = 0.99,
                AgeMat = 17, nages = 19,
                z = 2.39, lambdaMax = 1.04, K1plus = 9000
              ),
              fixed.cv.catch.w = 0
            ))
          })

test_that("Check that errors produced when params outside of bounds",
          {
            expect_error(rebuild_by_x(
              needf.start = 0.001,
              init.depl.w = 0.6, goal.w = 4500,
              desired.prob.w = 1.01, when.w = 100,
              lh.params.w = list(
                S0 = 0.944, S1plus = 0.99,
                AgeMat = 17, nages = 19,
                z = 2.39, lambdaMax = 1.04, K1plus = 9000
              ),
              fixed.cv.catch.w = 0
            ))
          })
