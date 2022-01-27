# Test nums per recruit function
test_that("NPR produces a list", {
  xx <- npr(S0 = .91, S1plus = .99, nages = 10, AgeMat = 8)
  expect_type(object = xx, type = "list")
})

test_that("One-plus nums per recruit is greater than reproducing nums per recruit", {
  xx <- npr(S0 = .91, S1plus = .99, nages = 10, AgeMat = 8)
  expect_true(xx$P1r > xx$npr)
})
