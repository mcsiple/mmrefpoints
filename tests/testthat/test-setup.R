# Test the setup file (make sure translator worked etc.)
test_that("Shiny i18n object is a translator",
         {expect_equal(class(i18n)[1], "Translator")})

test_that("Thumbnail 1 label object is a function",
          {expect_type(object = thumbnail_label2,type = "closure")})

test_that("Jumbotron text object is a function",
          {expect_type(object = jumbotron2,type = "closure")})
