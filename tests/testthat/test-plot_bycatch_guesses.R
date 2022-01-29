# testing plot_bycatch_guesses

test_that("Bycatch guesses produces ggplot object", {
  x <- plot_bycatch_guesses(highval = 55, medval = 35, lowval = 15, cv = 0.5, set_size = 12, color.palette = c("red", "yellow", "green"), lang = "en")
  expect_true(ggplot2::is.ggplot(x))
})

test_that("Plot object contains correct language", {
  x <- plot_bycatch_guesses(highval = 55, medval = 35, lowval = 15, cv = 0.5, set_size = 12, color.palette = c("red", "yellow", "green"), lang = "es")
  expect_equal(x$labels$y, "Mortalidad de captura incidental")
})
