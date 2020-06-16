#' Create pH plots to display status and trend.
#'
#'
#' @param data Dataframe to determine status from. Must have 'excursion' column generated.
#' @param seaKen Results of Seasonal Kendall Analysis
#' @param station The station to plot
#' @param max_date The max date to show on the plot
#' @return dataframe of stations with sufficient data
#' @export
#' @examples
#' plot_pH(data = data.frame, seaKen, station)

plot_pH <- function(data, seaKen, station, max_date = min(data$sample_datetime, na.rm = TRUE)){
  # subset seaken table to parameter and significant trends
  seaken_pH <- seaKen %>% dplyr::filter(Char_Name == "pH",
                                        significance != "No Significant Trend",
                                        MLocID == station)

  # obtain data range limits for plotting
  result_max <- max(c(data$Result_cen, data$pH_Max), na.rm = TRUE)
  xmin <- min(data$sample_datetime, na.rm = TRUE)
  xmax <- max_date
  ymin <- 0
  ymax <- ifelse(result_max > 11.5, result_max, 11.5)
  data$excursion <- dplyr::if_else(data$pH_excursion == 1, "Excursion", "Result") # change numeric value to descriptor

  # obtain plotting values for trend line if applicable
  if(nrow(seaken_pH) > 0){
    slope <- round(seaken_pH[, "slope"], digits=3)
    trend <- seaken_pH[, "trend"]
    p_val <- round(seaken_pH[, "p_value"], digits=3)
    x_delta <- as.numeric((xmax-xmin)/2)
    y_median <- median(data$Result_cen, na.rm = TRUE)
    sk_min <- y_median - x_delta*slope/365.25
    sk_max <- y_median + x_delta*slope/365.25
  }

  p <- ggplot2::ggplot(data)

  # add pH min and max criteria lines
  p <- p + ggplot2::geom_segment(aes(x=xmin, xend=xmax, y=pH_Min, yend=pH_Min,
                                     color = "pH Criteria", linetype = "pH Criteria", shape = "pH Criteria"))
  p <- p + ggplot2::geom_segment(aes(x=xmin, xend=xmax, y=pH_Max, yend=pH_Max,
                                     color = "pH Criteria", linetype = "pH Criteria", shape = "pH Criteria"))

  title <- paste(station, unique(data$StationDes))
  subtitle <- paste0("Assessment Unit: ", unique(data$AU_ID), " ", unique(data$AU_Name))

  # plot data with excursion colors
  p <- p + ggplot2::geom_point(aes(x=sample_datetime, y=Result_cen, color = excursion, linetype = excursion, shape = excursion)) +
    ggplot2::ggtitle(title, subtitle = subtitle) +
    ggplot2::ylab("pH (s.u.)") +
    ggplot2::xlab("Datetime")

  # plot the trend line if applicable
  if(nrow(seaken_pH) > 0){
    p <- p + ggplot2::geom_segment(aes(x=xmin, xend=xmax, y=sk_min, yend=sk_max, color = "Trend", linetype = "Trend", shape = "Trend"), lwd = 1) +
      ggplot2::annotate("text", x = xmin, y = ymax, label = paste0("Trend Results: ", trend, ",  Z-Stat: ", p_val, ",  Slope: ", slope), hjust = 0, vjust = 0)
  }

  # apply color, shape, line types, and range limits
  p <- p +
    ggplot2::scale_color_manual(name = "",
                                values =    c('Excursion' = 'red', 'Result' = 'black', "Trend" = 'blue', "pH Criteria" = 'black')) +
    ggplot2::scale_linetype_manual(name = "",
                                   values = c('Excursion' = 0, 'Result' = 0, "Trend" = 2, "pH Criteria" = 1)) +
    ggplot2::scale_shape_manual(name = "",
                                values =    c('Excursion' = 4, 'Result' = 16, "Trend" = 32, "pH Criteria" = 32)) +
    ggplot2::ylim(c(ymin, ymax)) +
    ggplot2::xlim(c(xmin, xmax)) +
    ggplot2::scale_x_datetime(date_labels = "%b-%Y")+
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")

  return(p)
}
