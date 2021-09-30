test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("Language check with switch", {
  testServer(app_server, {
    session$setInputs(
      selected_language = "es",
      Nmin.usr = 1000, fr.usr = 0.5, Rmax.usr = 1.01
    )
    x <- output$PBRprint
    y <- substr(x, 1, 10)
    expect_equal(y, "Rmax debe ")
  })
})

# test_that("Output exists when MNPL is high", {
#   testServer(app_server, {
#     session$setInputs(
#       type_simple = "bowhead",
#       MNPL.usr = 0.7
#     )
#     x <- output$projPlot1
#     expect_true(is.ggplot(x))
#   })
# })



