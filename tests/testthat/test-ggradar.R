test_that("check that ggradar produces ggplot object",
          {
            plot.data <- data.frame(
              bycatch = factor(c(
                "Higher end of bycatch range",
                "Midpoint of bycatch range",
                "Lower end of bycatch range"
              ),
              ,levels = c(
                "Higher end of bycatch range",
                "Midpoint of bycatch range",
                "Lower end of bycatch range"
              )),
              prebuild50 = c(0, 0, 1),
              prebuild100 = c(0, 0, 1),
              abundrel10 = c(0, 0, 0.29),
              abundrel20 = c(0, 0, 0.34),
              abundrel50 = c(0, 0, 0.53)
            )
            
            p1 <- ggradar(
              plot.data = plot.data,
              axis.label.size = 4,
              grid.label.size = 4,
              palette.vec = c("#99D594","#E6F598", "#FEE08B") 
            )
            
            expect_true(class(p1)[2]=="ggplot")
            
          })
