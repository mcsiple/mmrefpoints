# test add trans
test_that("add_trans() returns valid color code",
          {
            xx <- add_trans("red", 100)
            expect_true(grepl(pattern = "^#([A-Fa-f0-9]{8}|[A-Fa-f0-9]{3})$",x = xx))
          })
