# Plot projections for multiple depletion levels

#' multiplot.proj
#' @description plots outputs from several projections that result from \code{Projections()}, with multiple depletion levels.
#'
#' @param years.plot number of years to plot
#' @param spaghetti either FALSE or a number, where the number is how many simulations to show from the same scenario
#' @param years.to.plot How many years to plot on the x axis
#' @param K1plus carrying capacity in terms of the 1+ component of the population
#' @param InitDepls a vector of initial depletion levels; if the user has selected "show multiple depletion levels" in the app, this vector is automatically the depletion they entered, 75 percent of that value ("low" depletion) and 125 percent of that value ("high depletion")
#' @param color.palette A vector of length 3 giving the color values for low, medium, and high bycatch mortality or bycatch mortality rate
#' @param high.d1 a list containing output from \code{Projections()} (including a matrix of simulation trajectories) - this corresponds to the highest bycatch level (thus "high") and the depletion level ( \code{d1} indicates the lowest starting depletion level)
#' @param med.d1 a list containing the middle bycatch value and lowest starting depletion
#' @param low.d1 a list containing the lowest bycatch value and lowest starting depletion
#' @param high.d2 a list containing the highest bycatch value and middle starting depletion
#' @param med.d2 a list containing the middle bycatch value and middle starting depletion
#' @param low.d2 a list containing the lowest bycatch value and middle starting depletion
#' @param high.d3 a list containing the highest bycatch value and highest starting depletion
#' @param med.d3 a list containing the middle bycatch value and highest starting depletion
#' @param low.d3 a list containing the lowest bycatch value and highest starting depletion
#' @param ylims plot y limits
#'
#' @return A plot of 50% and 90% confidence intervals of population projections if \code{spaghetti == FALSE} or a spaghetti plot with n individual projections if \code{spaghetti == n },  from \code{Projections()}.
#'
#' @export
multiplot_proj <- function(high.d1 = testing.list[[1]][3][[1]]$trajectories, # d1 is lowest depl, high is highest bycatch rate
                           med.d1 = testing.list[[1]][2][[1]]$trajectories,
                           low.d1 = testing.list[[1]][1][[1]]$trajectories,
                           high.d2 = testing.list[[2]][3][[1]]$trajectories,
                           med.d2 = testing.list[[2]][2][[1]]$trajectories,
                           low.d2 = testing.list[[2]][1][[1]]$trajectories,
                           high.d3 = testing.list[[3]][3][[1]]$trajectories, # d3 is the highest depl
                           med.d3 = testing.list[[3]][2][[1]]$trajectories,
                           low.d3 = testing.list[[3]][1][[1]]$trajectories,
                           years.plot = 50, # different from years.plot?
                           ylims,
                           spaghetti = FALSE,
                           years.to.plot = 50,
                           K1plus = 9000,
                           InitDepls = InitDepl.vec,
                           color.palette = c("forestgreen", "darkorange", "red",
                                             lang = "en")) {
  high.col <- color.palette[3]
  med.col <- color.palette[2]
  low.col <- color.palette[1]

  # Translation setup
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
                         "fr" = "Haut de gamme \nde prises accessoires") #FIX FRENCH FOR THIS ONE AND THE ONE BELOW!~

  sdf_labs_low <- switch(lang,
                         "en" = "Low end of \nbycatch range",
                         "es" = "Extremo inferior del rango \nde captura incidental",
                         "fr" = "Haut de gamme \nde prises accessoires")

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

  if(spaghetti){ #did user ask to see individual projections?
    ts.length <- years.plot
    spag.df1 <- data.frame(year = as.numeric(rep(1:(ts.length+1),times=spaghetti)),
                           sim = as.factor(rep(1:spaghetti,each=ts.length+1)),
                           high = as.vector(t(high.d1[1:spaghetti,1:(ts.length+1)]))/K1plus,
                           med = as.vector(t(med.d1[1:spaghetti,1:(ts.length+1)]))/K1plus,
                           low = as.vector(t(low.d1[1:spaghetti,1:(ts.length+1)]))/K1plus)

    spag.df2 <- data.frame(year = as.numeric(rep(1:(ts.length+1),times=spaghetti)),
                           sim = as.factor(rep(1:spaghetti,each=ts.length+1)),
                           high = as.vector(t(high.d2[1:spaghetti,1:(ts.length+1)]))/K1plus,
                           med = as.vector(t(med.d2[1:spaghetti,1:(ts.length+1)]))/K1plus,
                           low = as.vector(t(low.d2[1:spaghetti,1:(ts.length+1)]))/K1plus)

    spag.df3 <- data.frame(year = as.numeric(rep(1:(ts.length+1),times=spaghetti)),
                           sim = as.factor(rep(1:spaghetti,each=ts.length+1)),
                           high = as.vector(t(high.d3[1:spaghetti,1:(ts.length+1)]))/K1plus,
                           med = as.vector(t(med.d3[1:spaghetti,1:(ts.length+1)]))/K1plus,
                           low = as.vector(t(low.d3[1:spaghetti,1:(ts.length+1)]))/K1plus)

    sdf1 <- melt(spag.df1,id.vars=c('year','sim'))
    sdf1$sim <- as.factor(sdf1$sim)
    sdf1$depl.level <- InitDepls[1]

    sdf2 <- melt(spag.df2,id.vars=c('year','sim'))
    sdf2$sim <- as.factor(sdf2$sim)
    sdf2$depl.level <- InitDepls[2]

    sdf3 <- melt(spag.df3,id.vars=c('year','sim'))
    sdf3$sim <- as.factor(sdf3$sim)
    sdf3$depl.level <- InitDepls[3]

    megadf <- bind_rows(sdf1,sdf2,sdf3)

    megadf <- megadf %>%
      mutate(variable = recode(variable,
                               high = sdf_labs_hi,
                               med = sdf_labs_med,
                               low = sdf_labs_low))

    anno <- data.frame(x=ts.length*.8,y=1, # annotations
                       sim=1:3,
                       depl.level=InitDepls,
                       labels= paste0("N[0] == ",round(InitDepls,digits=2),"*K"))#

    pp <- ggplot(megadf, aes(x=year,y=value)) +
      xlim(0,ts.length) +
      geom_path(lwd=1,alpha=0.5,aes(group=sim,colour=variable)) +
      scale_colour_manual(bylvl_lab,values = rev(color.palette)) +
      theme_classic(base_size=18) +
      xlab(year_lab) +
      ylab(abund_lab) +
      ylim(0,1) +
      facet_wrap(~depl.level,labeller=label_bquote(.(depl.level)~N[0])) +
      geom_label(data=anno, aes(x=x,y=y,label=labels),parse=T) +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            legend.position = 'bottom')

    pp

  }else{
    probs <- c(0.05,0.25,0.5,0.75,0.95)

    # low med and high refer to low med and high bycatch
    summary.high <- apply(high.d1[,1:years.to.plot],MARGIN = 2,FUN = quantile,probs=probs,na.rm=T)
    summary.med <- apply(med.d1[,1:years.to.plot],MARGIN = 2,FUN = quantile,probs=probs,na.rm=T)
    summary.low <- apply(low.d1[,1:years.to.plot],MARGIN = 2,FUN = quantile,probs=probs,na.rm=T)
    ts.length <- ncol(summary.high)
    t.high <- data.frame(t(summary.high),blvl="high",year=1:years.to.plot)
    t.med <- data.frame(t(summary.med),blvl="med",year=1:years.to.plot)
    t.low <- data.frame(t(summary.low),blvl="low",year=1:years.to.plot)
    colnames(t.high)[1:5] <- colnames(t.med)[1:5] <- colnames(t.low)[1:5] <- c("lo90","lo50","median","hi50","hi90")
    all1 <- rbind(t.high,t.med,t.low)
    all1$depl.level <- InitDepls[1]


    summary.high <- apply(high.d2[,1:years.to.plot],MARGIN = 2,FUN = quantile,probs=probs,na.rm=T)
    summary.med <- apply(med.d2[,1:years.to.plot],MARGIN = 2,FUN = quantile,probs=probs,na.rm=T)
    summary.low <- apply(low.d2[,1:years.to.plot],MARGIN = 2,FUN = quantile,probs=probs,na.rm=T)
    ts.length <- ncol(summary.high)
    t.high <- data.frame(t(summary.high),blvl="high",year=1:years.to.plot)
    t.med <- data.frame(t(summary.med),blvl="med",year=1:years.to.plot)
    t.low <- data.frame(t(summary.low),blvl="low",year=1:years.to.plot)
    colnames(t.high)[1:5] <- colnames(t.med)[1:5] <- colnames(t.low)[1:5] <- c("lo90","lo50","median","hi50","hi90")
    all2 <- rbind(t.high,t.med,t.low)
    all2$depl.level <- InitDepls[2]

    summary.high <- apply(high.d3[,1:years.to.plot],MARGIN = 2,FUN = quantile,probs=probs,na.rm=T)
    summary.med <- apply(med.d3[,1:years.to.plot],MARGIN = 2,FUN = quantile,probs=probs,na.rm=T)
    summary.low <- apply(low.d3[,1:years.to.plot],MARGIN = 2,FUN = quantile,probs=probs,na.rm=T)
    ts.length <- ncol(summary.high)
    t.high <- data.frame(t(summary.high),blvl="high",year=1:years.to.plot)
    t.med <- data.frame(t(summary.med),blvl="med",year=1:years.to.plot)
    t.low <- data.frame(t(summary.low),blvl="low",year=1:years.to.plot)
    colnames(t.high)[1:5] <- colnames(t.med)[1:5] <- colnames(t.low)[1:5] <- c("lo90","lo50","median","hi50","hi90")
    all3 <- rbind(t.high,t.med,t.low)
    all3$depl.level <- InitDepls[3]

    tp <- bind_rows(all1,all2,all3)

    tp <- tp %>%
      mutate(blvl = as.factor(blvl)) %>%
      mutate(blvl = recode_factor(blvl,
                                  high = sdf_labs_hi,
                                  med = sdf_labs_med,
                                  low = sdf_labs_low))

    anno <- data.frame(x=years.plot*.8,y=1,
                       depl.level=InitDepls,
                       labels= paste0("N[0] == ",round(InitDepls,digits=2),"*K"))#

    p <-
      ggplot(tp,aes(x=year)) +
      geom_ribbon(aes(ymin=lo90/K1plus,ymax=hi90/K1plus,fill=blvl),alpha=0.5) +
      geom_ribbon(aes(ymin=lo50/K1plus,ymax=hi50/K1plus,fill=blvl),alpha=0.5) +
      geom_line(aes(y=median/K1plus,colour=blvl),lwd=1.1) +
      ylim(0,1) +
      scale_fill_manual(bylvl_lab,values = rev(color.palette)) +
      scale_colour_manual(bylvl_lab,values = rev(color.palette)) +
      xlab(year_lab) +
      ylab(paste(abund_lab)) +
      labs(title = plottitle,
           subtitle = plotsubtitle,
           caption = plotcaption) +
      theme_classic(base_size=18) +
      facet_wrap(~depl.level,labeller=label_bquote(.(depl.level)~N[0])) +
      geom_label(data=anno, aes(x=x,y=y,label=labels),parse=T) +
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = 'bottom')
    p
  }
}
# multiplot.proj(spaghetti=10)
