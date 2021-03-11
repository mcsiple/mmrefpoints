#' Plot all the pinniped life history parameters
#' @description Make the table on the documentation tab of the app, which shows the range of pinniped life history parameters
#' @param dat a dataframe with all the parameters in in (from Pinniped parameters - Data.csv)
#' @param central Whether or not to plot a line showing the central tendency of all the parameters
#' @param set_size Base size for plot
#'
#' @return
#' @export
#'
#' @examples
#' plot_pinnipeds(dat = dat)
plot_pinnipeds <- function(dat, central = FALSE, set_size = 14) {
  if (length(unique(dat$Species)) != 24) {
    stop("your csv has too many species in it-- this will ruin the labels")
  }
  dat$Value <- as.character(dat$Value)

  # Take values from range column and put them in their own columns
  dat$Range_lo <- gsub("-.*$", "", dat$Range) # first value in range
  dat$Range_hi <- gsub(".*-", "", dat$Range) # second value in range

  clean <- dat %>%
    mutate(
      Value = suppressWarnings(as.numeric(Value)),
      Range_lo = suppressWarnings(as.numeric(Range_lo)),
      Range_hi = suppressWarnings(as.numeric(Range_hi))
    ) %>%
    mutate(Species = str_replace(Species, pattern = "\\(", replacement = "\\(\\<i\\>")) %>%
    mutate(Species = str_replace(Species, pattern = "\\)", replacement = "\\<\\/i\\>\\)")) %>%
    subset(Parameter %in% c("AgeMat", "S0", "S1plus")) %>%
    mutate(Species = factor(Species)) %>%
    as.data.frame()

  summary <- clean %>%
    group_by(Parameter) %>%
    summarise(param.mean = mean(Value, na.rm = T)) %>%
    as.data.frame()

  clean <- merge(clean, summary, by = "Parameter") %>%
    mutate_at(vars(Value), list(name = ~ round(., 2))) %>%
    mutate(value = round(Value, digits = 2))

  colourCount <- length(unique(dat$Species))
  getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Paired"))
  labs <- c(
    "S<sub>0</sub>",
    "S<sub>1+</sub>",
    "Age<sub>maturity</sub>"
  )

  pps1 <- clean %>%
    filter(Parameter %in% c("AgeMat", "S0", "S1plus")) %>%
    droplevels() %>%
    mutate(newParameter = factor(Parameter,
      levels = c("S0", "S1plus", "AgeMat"),
      labels = labs
    )) %>%
    ggplot(aes(
      x = Species, y = value,
      colour = factor(Species),
      group = Citation
    )) +
    geom_point(size = 2) +
    geom_linerange(aes(x = Species, ymin = Range_lo, ymax = Range_hi), lwd = 1.5, alpha = 0.5) +
    scale_colour_manual(values = c(getPalette(colourCount), rep("black", times = 4))) +
    facet_grid(~newParameter, scales = "free_x") +
    xlab("") +
    ylab("Parameter value") +
    {
      if (central == TRUE) {
        geom_hline(
          data = summary,
          mapping = aes(yintercept = param.mean),
          lwd = 0.75, colour = "darkgrey"
        )
      }
    } +
    theme_classic(base_size = set_size) +
    theme(
      strip.background = element_blank(),
      legend.position = "none"
    ) +
    coord_flip()

  pps1y <- plotly::ggplotly(pps1, tooltip = c("y", "group"))

  return(pps1y)
}

# test
# plot.pinnipeds(dat=dat)
