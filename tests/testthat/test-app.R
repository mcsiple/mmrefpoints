test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

 test_that("reactives and output updates",{
            testServer(app_server, {
             session$setInputs(selected_language = "es",
                               Nmin.usr = 1000, fr.usr = 0.5, Rmax.usr = 1.01)
              x <- output$PBRprint
              y <- substr(x,1,10)
              expect_equal(y, "Rmax debe ")
            })
  })

