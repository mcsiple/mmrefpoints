test_that("Shiny app generated from run_app()", {
  xx <- class(shiny::shinyApp(
         ui = app_ui, 
         server = app_server
     ))
  expect_equal("shiny.appobj", xx)
})
