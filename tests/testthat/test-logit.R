# test the logit and inv-logit functions
test_that("Logit transform of 0.5 is 0",
          {expect_equal(0, logit(0.5))
            })

test_that("Inverse logit transform of 0 is 0.5",
  {expect_equal(0.5, inv_logit(0))
    })