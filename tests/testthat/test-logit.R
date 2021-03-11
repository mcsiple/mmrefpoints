# test the inv-logit function
test_that("Logit transform of 0.5 is 0",
          {expect_equal(0, logit(0.5))
            })