#' Plot projections
#' @description plots outputs from several projections that result from a Projections() call.
#'
#' @param high a list containing output from \code{Projections()} (including a matrix of simulation trajectories) - corresponding to a high bycatch level (this is at the high end of the range defined by the user in the Shiny app)
#' @param med a list containing output from \code{Projections()} (including a matrix of simulation trajectories) - corresponding to a medium bycatch level (this is at the median of the low and high end of the range defined by the user)
#' @param low a list containing output from \code{Projections()} (including a matrix of simulation trajectories) - corresponding to a high bycatch level (this is at the high end of the range defined by the user)
#' @param years.plot number of years to plot on the x axis
#' @param spaghetti either FALSE or a number, where the number is how many simulations to show from the same scenario
#' @param ylims y-limits of projection plot
#' @param K1plus carrying capacity in terms of age 1+ individuals
#' @param InitDepl initial depletion level (set by the user)
#' @param color.palette a vector of three colors to use for low, medium and high bycatch rates
#' @param lang language selected by the user (character)
#'
#' @return A plot of 50 percent and 90 percent confidence intervals of population projections (if \code{spaghetti == FALSE}) or a spaghetti plot (if \code{spaghetti ==TRUE}),  from \code{Projections()}.
#' @examples 
#' parms <- list(S0=0.944,S1plus=0.99,
#' K1plus=9000,AgeMat=18,PlusGroupAge=25,z= 2.39,lambdaMax=1.02)
#' initdepl <- 0.5
#' high.simple <- projections(
#'   NOut = 50,
#'   ConstantBycatch = list(
#'     Catch = 100,
#'    CV = 0.3
#'   ),
#'   InitDepl = initdepl,
#'   lh.params = parms,
#'   nyears = 100)
#' med.simple <- projections(
#'   NOut = 50,
#'   ConstantBycatch = list(
#'     Catch = 50,
#'     CV = 0.3
#'   ),
#'  InitDepl = initdepl,
#'   lh.params = parms,
#'   nyears = 100)
#' low.simple <- projections(
#'   NOut = 50,
#'   ConstantBycatch = list(
#'     Catch = 10,
#'     CV = 0.3
#'   ),
#'   InitDepl = initdepl,
#'   lh.params = parms,
#'   nyears = 100)

#' x <- plot_proj(high = high.simple,
#' med = med.simple,
#' low = low.simple,
#' years.plot = 50,
#' ylims = c(0, parms$K1plus),InitDepl = initdepl, 
#' K1plus =parms$K1plus)
#' 
#' x
#' 
#' @export
plot_proj <- function(high,
                      med,
                      low,
                      years.plot = 50,
                      ylims,
                      spaghetti = FALSE,
                      K1plus = 9000,
                      InitDepl = 0.8,
                      color.palette = c("#7bbcb0", "#3a7c89", "#123f5a"),
                                        lang = "en") {
  high.col <- color.palette[3]
  med.col <- color.palette[2]
  low.col <- color.palette[1]

  plottitle <- switch(lang,
                      "en" = "Model projections",
                      "es" = "Proyecciones",
                      "fr" = "Projections")

  plotsubtitle <- switch(lang,
                         "en" = "Median population size and quantiles",
                         "es" = "Tamaño medio de la población y cuartiles",
                         "fr" = "Taille médiane de la population et quantiles")

  plotcaption <- switch(lang,
                        "en" = "95%: lightly shaded; 50%: heavily shaded",
                        "es" = "95%: ligeramente sombreado; 50%: fuertememente sombreado",
                        "fr" = "95%: légèrement ombragé; 50%: très ombragé")

  sdf_labs_hi <- switch(lang,
                        "en" = "High end of \nbycatch range",
                        "es" = "Extremo superior del rango \nde captura incidental",
                        "fr" = "Haut de gamme \nde prises accessoires")

  sdf_labs_med <- switch(lang,
                         "en" = "Midpoint of \nbycatch range",
                         "es" = "Rango medio \nde captura incidental",
                         "fr" = "Medium de gamme \nde prises accessoires") #FIX FRENCH FOR THIS ONE AND THE ONE BELOW!~

  sdf_labs_low <- switch(lang,
                         "en" = "Low end of \nbycatch range",
                         "es" = "Extremo inferior del rango \nde captura incidental",
                         "fr" = "Lower de gamme \nde prises accessoires")

  bylvl_lab <- switch(lang,
                      "en" = "Bycatch level",
                      "es" = "Nivel de captura incidental",
                      "fr" = "Niveau de prises accessoires")

  year_lab <- switch(lang,
                     "en" = "Year",
                     "es" = "Año",
                     "fr" = "Année")

  abund_lab <- switch(lang,
                      "en" = "Abundance  (N/K)",
                      "es" = "Tamaño poblacional relativo a K",
                      "fr" = "Abondance (N/K)")

  if(spaghetti){
    ts.length <- years.plot
    spag.df <- data.frame(year = as.numeric(rep(1:(ts.length+1),times=spaghetti)),
                          sim = as.factor(rep(1:spaghetti,each=ts.length+1)),
                          high = as.vector(t(high$trajectories[1:spaghetti,1:(ts.length+1)]))/K1plus,
                          med = as.vector(t(med$trajectories[1:spaghetti,1:(ts.length+1)]))/K1plus,
                          low = as.vector(t(low$trajectories[1:spaghetti,1:(ts.length+1)]))/K1plus
    )

    sdf <- reshape2::melt(spag.df,id.vars=c('year','sim'))
    sdf$sim <- as.factor(sdf$sim)
    dlab1 <- paste("N[0] == ",round(InitDepl,digits=2),"*K")

    sdf <- sdf %>% mutate(variable = recode(variable,
                                            high = sdf_labs_hi,
                                            med = sdf_labs_med,
                                            low = sdf_labs_low))

    p <- sdf %>%
      ggplot(aes(x=year,y=value,group=sim,colour=variable)) +
      xlim(0,ts.length)+
      geom_path(lwd=1,alpha=0.5) +
      scale_colour_manual(bylvl_lab,values = rev(color.palette)) +
      theme_classic(base_size=18) +
      xlab(year_lab) + ylab(abund_lab) +ylim(0,1)+
      annotate("label", x=ts.length*.8, y=.95, label = dlab1,parse=TRUE,size=6) +
      labs(title = plottitle,
           subtitle = plotsubtitle,
           caption = plotcaption) +
      theme(legend.position = "bottom",
            legend.direction = "vertical",
            axis.text.x = element_text(size=18),
            axis.text.y = element_text(size=18),
            axis.title.y = element_text(size=20))

    p

  }else{
    probs <- c(0.05,0.25,0.5,0.75,0.95)

    # low med and high refer to low med and high bycatch
    summary.high <- apply(high$trajectories[,1:years.plot],MARGIN = 2,FUN = quantile,probs=probs,na.rm=T)
    summary.med <- apply(med$trajectories[,1:years.plot],MARGIN = 2,FUN = quantile,probs=probs,na.rm=T)
    summary.low <- apply(low$trajectories[,1:years.plot],MARGIN = 2,FUN = quantile,probs=probs,na.rm=T)
    ts.length <- ncol(summary.high)


    t.high <- data.frame(t(summary.high),blvl="high",year=1:years.plot)
    t.med <- data.frame(t(summary.med),blvl="med",year=1:years.plot)
    t.low <- data.frame(t(summary.low),blvl="low",year=1:years.plot)
    all <- rbind(t.high,t.med,t.low)

    colnames(all) <- c("lo90","lo50","median","hi50","hi90","blvl","year")

    all <- all %>%
      mutate(blvl = factor(blvl)) %>%
      mutate(blvl = recode_factor(blvl,
                                  high = sdf_labs_hi,
                                  med = sdf_labs_med,
                                  low = sdf_labs_low))

    dlab <- paste("N[0] == ",round(InitDepl,digits=2),"*K")

    p <- all %>%
      ggplot(aes(x=year)) +
      geom_ribbon(aes(ymin=lo90/K1plus,ymax=hi90/K1plus,fill=blvl),alpha=0.5) +
      geom_ribbon(aes(ymin=lo50/K1plus,ymax=hi50/K1plus,fill=blvl),alpha=0.5) +
      geom_line(aes(y=median/K1plus,colour=blvl),lwd=1.1) +
      ylim(0,1) +
      scale_fill_manual(bylvl_lab,values = rev(color.palette)) +
      scale_colour_manual(bylvl_lab,values = rev(color.palette)) +
      xlab(year_lab) +
      ylab(abund_lab) +
      labs(title = plottitle,
           subtitle = plotsubtitle,
           caption = plotcaption) +
      theme_classic(base_size=18) +
      annotate("label", x=years.plot*.8, y=.95, label = dlab,parse=TRUE,size=6) +
      theme(legend.position = "bottom",
            legend.direction = "vertical",
            axis.text.x = element_text(size = 18),
            axis.text.y = element_text(size = 18),
            axis.title.y = element_text(size = 20),
            plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 16))
    p

  }
}
