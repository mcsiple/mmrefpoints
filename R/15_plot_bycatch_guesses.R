#' Plot distribution of bycatch values
#' @description Plot user-defined bycatch values (uses \code{geom_linerange()} from \code{ggplot2})
#' @param highval The high end of the user-defined range of bycatch values
#' @param medval The middle of the user-defined bycatch range
#' @param lowval The low end of the user-defined range
#' @param cv CV of bycatch mortality or bycatch mortality rate
#' @param set_size base size to pass to \code{ggplot}
#' @param print.tiff \code{logical} whether or not to write a tiff file to \code{outputdir}
#' @param color.palette A vector of colors to represent bycatch levels, from low to high end of range
#' @param outputdir If \code{print.tiff = TRUE}, the directory to place tiff file of plot
#' @return A \code{ggplot2} grob showing the distributions of bycatch values based on what the user entered.
#'
#' @export
plot_bycatch_guesses <- function(highval, medval, lowval,
                                 cv, set_size = 18, print.tiff = FALSE,
                                 color.palette = c("forestgreen", "orange", "red"),
                                 outputdir = "/Users/mcsiple/Dropbox/OMFMarineMammals/Talks",
                                 lang = "en") {
  ylab <- switch(lang,
                 "en" = "Bycatch or bycatch rate",
                 "es" = "Mortalidad de captura incidental \n o tasa de mortalidad por captura incidental",
                 "fr" = "Mortalité par prise accessoire \n ou taux de prises accessoires")
  caption <- switch(lang,
                    "en" = "50% and 95% quantiles shown as colored and grey lines respectively",
                    "es" = "50% y 95% cuantiles mostrados como líneas de color y grises respectivamente",
                    "fr" = "50% et 95% quantiles représentés respectivement par des lignes colorées et grises"
  )
  legend_title <- switch(lang,
                         "en" = "Bycatch level",
                         "es" = "Niveles de mortalidad",
                         "fr" = "Niveaux de mortalité")
  level_names <- switch(lang,
                        "en" = c("Low","Med","High"),
                        "es" = c("Bajo","Medio","Alto"),
                        "fr" = c("Inférieure","Médian","Supérieure"))

  sdlog.usr <- sqrt(log((cv^2)+1)) # should be ~cv when sd is low (<0.5)
  high <- rlnorm(1000,meanlog = log(highval),sdlog = sdlog.usr)
  med <- rlnorm(1000,meanlog = log(medval),sdlog = sdlog.usr)
  low <- rlnorm(1000,meanlog = log(lowval),sdlog = sdlog.usr)
  dat <- data.frame(high, med, low)
  md <- t(apply(dat,2,FUN = quantile, probs = c(0.025,0.25,0.5,0.75,0.975))) %>% as.data.frame()
  colnames(md) <- c('q2.5','q25','q50','q75','q97.5')
  md$bycatch.rate <- c("high","med","low")
  md$bycatch.rate <- factor(md$bycatch.rate,levels=c("low","med","high"))
  md$bycatch.rate <- recode(md$bycatch.rate,low=level_names[1],
                            med=level_names[2],
                            high=level_names[3])
  pd = position_dodge(0.25)

  r <-  ggplot(md, aes(x = bycatch.rate, y = q50),position = pd) +
    geom_errorbar(width = 0.02,
                  aes(ymin = q2.5, ymax = q97.5,group = bycatch.rate),
                  colour="darkgrey",position=pd,size=1.1) + #95% intervals
    geom_errorbar(width = 0.02, size=2.1,
                  aes(ymin = q25, ymax = q75,colour = bycatch.rate),
                  alpha=0.85,position=pd) +   #50% intervals
    geom_point(shape = 21, size = 6,aes(fill = bycatch.rate),position=pd) +
    theme_classic(base_size=set_size) +
    scale_fill_manual(legend_title, values = color.palette) +
    scale_colour_manual(legend_title, values = color.palette) +
    theme(axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=18),
          axis.title.y = element_text(size=20),
          legend.position = "bottom") +
    coord_cartesian(ylim=c(0,max(md$q97.5))) +
    xlab("") +
    ylab(ylab) +
    labs(caption = caption)

  if(print.tiff){
    tiff(filename = paste(outputdir,"/bycatch_intervals.tiff",sep=""),
         width = 5, height = 4.5,
         units = 'in',res=100)
    print(r)
    dev.off()
  }else{return(r)}
}


# p <- plot_bycatch_guesses(highval = 150, medval = 75,lowval = 20,cv=.10)
