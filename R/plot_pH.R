#' Create pH plots to display status and trend.
#'
#'
#' @param data Dataframe to determine status from. Must have 'excursion' column generated.
#' @param seaKen Results of Seasonal Kendall Analysis
#' @param station The station to plot
#' @return dataframe of stations with sufficient data
#' @export
#' @examples
#' plot_pH(data = data.frame, seaKen, station)

plot_pH <- function(data, seaKen, station){
  # subset seaken table to parameter and significant trends
  seaken_pH <- seaKen %>% filter(Char_Name == "pH",
                                 significance != "No Significant Trend",
                                 MLocID == station)

  # obtain data range limits for plotting
  xmin <- min(data$sample_datetime, na.rm = TRUE)
  xmax <- max(data$sample_datetime, na.rm = TRUE)
  ymin <- min(c(data$Result_Numeric, data$pH_Min), na.rm = TRUE)
  ymax <- max(c(data$Result_Numeric, data$pH_Max), na.rm = TRUE)
  data$excursion <- if_else(data$pH_excursion == 1, "Excursion", "Result") # change numeric value to descriptor

  # obtain plotting values for trend line if applicable
  if(nrow(seaken_pH) > 0){
    slope <- seaken_pH[, "slope"]
    x_delta <- as.numeric((xmax-xmin)/2)
    y_median <- median(data$Result_Numeric, na.rm = TRUE)
    sk_min <- y_median - x_delta*slope/365.25
    sk_max <- y_median + x_delta*slope/365.25
  }

  p <- ggplot(data)

  # add pH min and max criteria lines
  p <- p + geom_segment(aes(x=xmin, xend=xmax, y=pH_Min, yend=pH_Min,
                            color = "pH Criteria", linetype = "pH Criteria", shape = "pH Criteria"))
  p <- p + geom_segment(aes(x=xmin, xend=xmax, y=pH_Max, yend=pH_Max,
                            color = "pH Criteria", linetype = "pH Criteria", shape = "pH Criteria"))

  # plot data with excursion colors
  p <- p + geom_point(aes(x=sample_datetime, y=Result_cen, color = excursion, linetype = excursion, shape = excursion)) +
    ggtitle(paste(station, "pH"), subtitle = paste(unique(data$StationDes))) +
    ylab("pH") +
    xlab("Datetime")

  # plot the trend line if applicable
  if(nrow(seaken_pH) > 0){
    p <- p + geom_segment(aes(x=xmin, xend=xmax, y=sk_min, yend=sk_max, color = "Trend", linetype = "Trend", shape = "Trend"))
  }

  # apply color, shape, line types, and range limits
  p <- p +
    scale_color_manual(name = "Legend",
                       values =    c('Excursion' = 'red', 'Result' = 'black', "Trend" = 'blue', "pH Criteria" = 'black')) +
    scale_linetype_manual(name = "Legend",
                          values = c('Excursion' = 0, 'Result' = 0, "Trend" = 1, "pH Criteria" = 2)) +
    scale_shape_manual(name = "Legend",
                       values =    c('Excursion' = 16, 'Result' = 16, "Trend" = 32, "pH Criteria" = 32)) +
    ylim(c(ymin, ymax)) +
    xlim(c(xmin, xmax)) +
    scale_x_datetime(date_labels = "%b-%Y")+
    theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")

  return(p)
}
