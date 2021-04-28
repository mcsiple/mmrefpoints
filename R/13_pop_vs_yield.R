#' Yield or productivity curve
#' @description relative population size (x) vs. Sustainable yield (y)
#'
#' @param z.vec a vector of z values
#' @param lh.params a list of life history parameters
#' @param add.legend logical; whether or not to add a legend
#' @param ggp logical; whether to plot in ggplot and plot a single yield curve; set to \code{FALSE} for base R plot and multiple yield curves.
#' @param linecolor color of yield curve line
#' @param lang language selected by the user (character)
#'
#' @return a plot of population size vs. 'yield'
#'
#' @examples
#' pop_vs_yield(z.vec = c( 1.1, 2.5, 5.99),
#' lh.params = list(
#'   S0 = 0.944, S1plus = 0.99, AgeMat = 17,
#'   nages = 19, lambdaMax = 1.04
#' ), ggp = FALSE)
#' @export
#'
pop_vs_yield <- function(z.vec = c(1, 2.39, 5.99),
                         lh.params = lh.params1,
                         add.legend = FALSE,
                         ggp = TRUE,
                         linecolor = "#123f5a",
                         lang = "en") {
  
  # Check z param values
  if (any(z.vec < -0.5) | any(z.vec > 6)) {
    stop("One of the z values is out of range; try a value for MNPL that is between 0 and 1.")
  }
  
  # Labels
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
  nages <- lh.params$nages
  lambdaMax <- lh.params$lambdaMax

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
  
  # paste in
  z.list <- list()
  for (j in 1:length(z.vec)) {
    z <- z.vec[j]
    yield.vec <- vector()
    rel1plus <- vector()

    # unfished nums per recruit
    u1p <- P0
    # u1p <- npr(S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, E = 0)$P1r *
    #   get_rf(E_in = 0, S0 = S0, S1plus = S1plus, nages = nages, AgeMat = AgeMat, z = z, A = A, P0 = P0, N0 = N0) # == 1

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
    if (j == 1) {
      maxyield <- which.max(yield.vec)
      print(paste("approx MNPL", round(rel1plus[maxyield], digits = 2)))
      print(paste("approx FMSY", round(E.vec[maxyield], digits = 5)))
    }

    z.list[[j]] <- data.frame(z = z, E = E.vec, yield = yield.vec, rel1p = rel1plus)
  } # / j loop
  dat <- bind_rows(z.list, .id = "column_label")

  if (ggp) {
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
      scale_x_continuous(limits = c(0, 1.5), expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      xlab(xlab1) + # "Depletion \n(population size relative to K)",
      ylab(ylab1) # Production
    return(p)
  } else {
    plot(0:1, 0:1,
      type = "n",
      xlab = xlab1,
      ylab = ylab1,
      xlim = c(0, 1.5),
      ylim = c(0, max(dat$yield)*1.2), 
      yaxs="i"
    )
    maxyield <- MNPL <- FMSY <- vector()
    for (z in 1:length(z.vec)) {
      col_vec <- c("lightblue", "blue", "darkblue")
      x_values <- dat[which(dat$column_label == z), "rel1p"]
      y_values <- dat[which(dat$column_label == z), "yield"]
      lines(x_values[1:n2p], y_values[1:n2p], col = col_vec[z])
      maxyield[z] <- which.max(y_values)
      MNPL[z] <- x_values[maxyield][1]
      FMSY[z] <- dat[which(dat$column_label == z)[maxyield[z]], "E"][1]
    }
    MNPL <- round(MNPL, digits = 2)
    FMSY <- round(FMSY, digits = 2)
    legend("topright", legend = paste0("z = ", z.vec, "\napprox MNPL = ", MNPL, "\napprox FMSY = ", FMSY, "\n"), lty = 1, col = col_vec, box.lwd = 0)
    return(data.frame(z = z.vec, MNPL = MNPL, FMSY = FMSY))
  } # /base R
}
