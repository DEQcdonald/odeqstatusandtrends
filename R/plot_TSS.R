#' Create Total Suspended Solids plots to display status and trend.
#'
#'
#' @param data Dataframe to determine status from. Must have 'excursion' column generated.
#' @param seaKen Results of Seasonal Kendall Analysis
#' @param station The station to plot
#' @return dataframe of stations with sufficient data
#' @export
#' @examples
#' plot_pH(data = data.frame, seaKen, station)

plot_TSS <- function(data, seaKen, station){
  # subset seaken table to parameter and significant trends
  seaken_TSS <- seaKen %>% filter(Char_Name == "Total suspended solids",
                                 significance != "No Significant Trend",
                                 MLocID == station)

  # obtain data range limits for plotting
  xmin <- min(data$sample_datetime, na.rm = TRUE)
  xmax <- max(data$sample_datetime, na.rm = TRUE)
  ymin <- min(c(data$Result_Numeric, data$TSS_crit), na.rm = TRUE)
  ymax <- max(c(data$Result_Numeric, data$TSS_crit), na.rm = TRUE)
  data$excursion <- if_else(data$excursion_cen == 1, "Excursion", "Result") # change numeric value to descriptor

  # obtain plotting values for trend line if applicable
  if(nrow(seaken_TSS) > 0){
    slope <- seaken_TSS[, "slope"]
    x_delta <- as.numeric((xmax-xmin)/2)
    y_median <- median(data$Result_Numeric, na.rm = TRUE)
    sk_min <- y_median - x_delta*slope/365.25
    sk_max <- y_median + x_delta*slope/365.25
  }

  p <- ggplot(data)

  # add TSS target lines
  if(any(!is.na(data$TSS_crit))){
    p <- p + geom_segment(aes(x=xmin, xend=xmax, y=TSS_crit, yend=TSS_crit,
                              color = "TSS Target", linetype = "TSS Target", shape = "TSS Target"))
  }
  # plot data with excursion colors
  p <- p + geom_point(aes(x=sample_datetime, y=Result_Numeric, color = excursion, linetype = excursion, shape = excursion)) +
    ggtitle(paste(station, "TSS")) +
    ylab("TSS") +
    xlab("Datetime")

  # plot the trend line if applicable
  if(nrow(seaken_TSS) > 0){
    p <- p + geom_segment(aes(x=xmin, xend=xmax, y=sk_min, yend=sk_max, color = "Trend", linetype = "Trend", shape = "Trend"))
  }

  # apply color, shape, line types, and range limits
  p <- p +
    scale_color_manual(name = "Legend",
                       values =    c('Excursion' = 'red', 'Result' = 'black', "Trend" = 'blue', "TSS Target" = 'black')) +
    scale_linetype_manual(name = "Legend",
                          values = c('Excursion' = 0, 'Result' = 0, "Trend" = 1, "TSS Target" = 2)) +
    scale_shape_manual(name = "Legend",
                       values =    c('Excursion' = 16, 'Result' = 16, "Trend" = 32, "TSS Target" = 32)) +
    ylim(c(ymin, ymax)) +
    xlim(c(xmin, xmax)) +
    theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")

  return(p)
}
