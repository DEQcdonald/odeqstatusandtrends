#' Create bacteria plots to display status and trend.
#'
#'
#' @param data Dataframe to determine status from. Must have 'excursion' column generated.
#' @param seaKen Results of Seasonal Kendall Analysis
#' @param station The station to plot
#' @return dataframe of stations with sufficient data
#' @export
#' @examples plot_pH(data = data.frame, seaKen, station)

plot_bacteria <- function(data, seaKen, station){
  # subset seaken table to parameter and significant trends
  seaken_bact <- seaKen %>% filter(Char_Name %in% AWQMS_Char_Names("bacteria"),
                                   significance != "No Significant Trend",
                                   MLocID == station)

  # obtain data range limits for plotting
  xmin <- min(data$sample_datetime, na.rm = TRUE)
  xmax <- max(data$sample_datetime, na.rm = TRUE)
  ymin <- min(c(data$Result_Numeric, data$bact_crit_ss, data$bact_crit_geomean), na.rm = TRUE)
  ymax <- max(c(data$Result_Numeric, data$bact_crit_ss, data$bact_crit_geomean), na.rm = TRUE)
  data$excursion <- if_else(data$excursion_cen == 1, "Excursion", "Result") # change numeric value to descriptor

  # obtain plotting values for trend line if applicable
  if(nrow(seaken_bact) > 0){
    slope <- seaken_bact[, "slope"]
    x_delta <- as.numeric((xmax-xmin)/2)
    y_median <- median(data$Result_Numeric, na.rm = TRUE)
    sk_min <- y_median - x_delta*slope/365.25
    sk_max <- y_median + x_delta*slope/365.25
  }

  p <- ggplot(data)

  # add geomean and ss criteria lines
  if(any(!is.na(data$bact_crit_ss))){
    p <- p + geom_segment(aes(x=xmin, xend=xmax, y=bact_crit_ss, yend=bact_crit_ss,
                              color = "Single Sample Criteria", linetype = "Single Sample Criteria", shape = "Single Sample Criteria"))
  }
  if(any(!is.na(data$bact_crit_geomean))){
    p <- p + geom_segment(aes(x=xmin, xend=xmax, y=bact_crit_geomean, yend=bact_crit_geomean,
                              color = "Geomean Criteria", linetype = "Geomean Criteria", shape = "Geomean Criteria"))
  }
  # plot data with excursion colors
  p <- p + geom_point(aes(x=sample_datetime, y=Result_Numeric, color = excursion, linetype = excursion, shape = excursion)) +
    ggtitle(paste(station, "Bacteria")) +
    ylab("Bacteria") +
    xlab("Datetime")

  # plot the trend line if applicable
  if(nrow(seaken_bact) > 0){
    p <- p + geom_segment(aes(x=xmin, xend=xmax, y=sk_min, yend=sk_max, color = "Trend", linetype = "Trend", shape = "Trend"))
  }

  # apply color, shape, line types, and range limits
  p <- p +
    scale_color_manual(name = "Legend",
                       values =    c('Excursion' = 'red', 'Result' = 'black', "Trend" = 'blue',
                                     "Single Sample Criteria" = 'black', "Geomean Criteria" = 'black')) +
    scale_linetype_manual(name = "Legend",
                          values = c('Excursion' = 0, 'Result' = 0, "Trend" = 1,
                                     "Single Sample Criteria" = 2, "Geomean Criteria" = 2)) +
    scale_shape_manual(name = "Legend",
                       values =    c('Excursion' = 16, 'Result' = 16, "Trend" = 32,
                                     "Single Sample Criteria" = 32, "Geomean Criteria" = 32)) +
    ylim(c(ymin, ymax)) +
    xlim(c(xmin, xmax)) +
    theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")

  return(p)
}
