#' Yield or productivity curve
#' @description relative population size (x) vs. Sustainable yield (y)
#'
#' @param z.vec a vector of z values
#' @param lh.params a list of life history parameters
#' @param add.legend logical; whether or not to add a legend
#' @param ggp logical; whether to plot in ggplot; set to \code{FALSE} for base R plot.
#' @param linecolor color of yield curve line
#' @param lang language selected by the user (character)
#'
#' @return a plot of population size vs. 'yield'
#'
#' @examples
#' pop_vs_yield(lh.params = list(
#'   S0 = 0.944, S1plus = 0.99, AgeMat = 17,
#'   nages = 19, fmax = 0.29, z = 2.39, lambdaMax = 1.04, K1plus = 9000
#' ))
#' @export
#'
pop_vs_yield <- function(z.vec = c(1, 2.39, 5.99),
                         lh.params = lh.params1,
                         add.legend = FALSE,
                         ggp = TRUE,
                         linecolor = "#123f5a",
                         lang = "en") {
  xlab1 <- switch(lang,
    "en" = "Depletion \n(population size relative to K)",
    "es" = "Agotamiento \n(tamaño de la población en relación con K)",
    "fr" = "Niveaux d'épuisement \n (taille de la population par rapport à leur capacité de charge K)"
  )

  ylab1 <- switch(lang,
    "en" = "Production",
    "es" = "Producción",
    "fr" = "Production"
  )
  n2p <- 100
  S0 <- lh.params$S0
  S1plus <- lh.params$S1plus
  AgeMat <- lh.params$AgeMat
  PlusGroupAge <- lh.params$PlusGroupAge
  nages <- lh.params$nages
  lambdaMax <- lh.params$lambdaMax
  K1plus <- lh.params$K1plus

  # Other stuff that depends only on life history
  NPROut <- npr(
    S0 = S0,
    S1plus = S1plus,
    nages = nages,
    AgeMat = AgeMat,
    E = 0
  )
  P0 <- NPROut$P1r
  N0 <- NPROut$npr
  # unpr <- NPROut$npr
  Fec0 <- 1.0 / N0
  fmax <- getfecmax(
    lambdaMax = lambdaMax,
    S1plus = S1plus,
    S0 = S0,
    AgeMat = AgeMat
  )
  A <- (fmax - Fec0) / Fec0
  E.vec <- seq(0, 0.1, length.out = 100)
  if (ggp) {
    z.list <- list()
    for (j in 1:length(z.vec)) {
      z <- z.vec[j]
      yield.vec <- vector()
      rel1plus <- vector()

      # unfished nums per recruit
      u1p <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = 0)$P1r *
        get_rf(E_in = 0, S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, z = z, A = A, P0 = P0, N0 = N0) # == 1

      for (i in 1:length(E.vec)) {
        # yield at exploitation E
        yield.vec[i] <- ce(
          S0 = S0,
          S1plus = S1plus,
          nages = nages,
          AgeMat = AgeMat,
          z = z,
          E = E.vec[i], A = A, P0 = P0, N0 = N0
        )

        # nums per recruit at exploitation E
        n1p <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = E.vec[i])$P1r *
          get_rf(E_in = E.vec[i], S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, z = z, A = A, P0 = P0, N0 = N0)

        # relative 1+ pop size at E
        rel1plus[i] <- n1p / u1p
      }

      z.list[[j]] <- data.frame(z = z, E = E.vec, yield = yield.vec, rel1p = rel1plus)
    }
    dat <- bind_rows(z.list, .id = "column_label")

    p <- dat %>%
      filter(z == z.vec[1]) %>%
      ggplot(aes(x = rel1p, y = yield)) +
      geom_path(colour = linecolor, lwd = 1.2) +
      theme_classic(base_size = 14) +
      theme(
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)
      ) +
      scale_x_continuous(limits = c(0, 1.5)) +
      xlab(xlab1) + # "Depletion \n(population size relative to K)",
      ylab(ylab1) # Production
    return(p)
  } else {
    plot(0:1, 0:1,
      type = "n",
      xlab = xlab1,
      ylab = ylab1, ylim = c(0, 1.5), xlim = c(0, 2)
    )

    for (j in 1:length(z.vec)) {
      z <- z.vec[j]
      yield.vec <- vector()
      rel1plus <- vector()

      # unfished nums per recruit
      u1p <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = 0)$P1r *
        get_rf(E_in = 0, S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, z = z, A = A, P0 = P0, N0 = N0) # == 1

      for (i in 1:length(E.vec)) {
        # yield at exploitation E
        yield.vec[i] <- ce(
          S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, z = z,
          E = E.vec[i], A = A, P0 = P0, N0 = N0
        )
        # nums per recruit at exploitation E
        n1p <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = E.vec[i])$P1r *
          get_rf(E_in = E.vec[i], S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, z = z, A = A, P0 = P0, N0 = N0)
        # relative 1+ pop size at E
        rel1plus[i] <- n1p / u1p
      }
      lines(rel1plus[1:n2p], yield.vec[1:n2p], col = "darkblue")
      maxyield <- which.max(yield.vec)
      print(paste("approx MNPL", round(rel1plus[maxyield], digits = 2)))
      print(paste("approx FMSY", round(E.vec[maxyield], digits = 5)))
    }
    if (add.legend) {
      legend("topright",
        lty = c(1, 1, 1),
        col = cols,
        legend = c("z = 1", "z = 2.39", "z = 5.99")
      )
    }
  } # end base R version
}
