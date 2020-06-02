#' Create temperature plots to display status and trend.
#'
#'
#' @param data Dataframe to determine status from. Must have 'excursion' column generated.
#' @param seaKen Results of Seasonal Kendall Analysis
#' @param station The station to plot
#' @return dataframe of stations with sufficient data
#' @export
#' @examples
#' plot_temp_tmdl(data = data.frame, seaKen, station)

plot_temp_tmdl <- function(data, seaKen, station){
  # subset seaken table to parameter and significant trends
  seaken_temp <- seaKen %>% dplyr::filter(Char_Name == "Temperature, water",
                                         significance != "No Significant Trend",
                                         MLocID == station)

  # obtain data range limits for plotting
  result_max <- max(c(data$Result_cen, data$target_value), na.rm = TRUE)
  xmin <- min(data$sample_datetime, na.rm = TRUE)
  xmax <- max(data$sample_datetime, na.rm = TRUE)
  ymin <- 0
  ymax <- ifelse(result_max > 100, result_max, 100)
  data$excursion <- dplyr::if_else(!is.na(data$excursion_cen),
                                   dplyr::if_else(data$excursion_cen == 1, "Excursion", "Result"),
                                   "Result") # change numeric value to descriptor

  # obtain plotting values for trend line if applicable
  if(nrow(seaken_temp) > 0){
    slope <- round(seaken_temp[, "slope"], digits=3)
    trend <- seaken_temp[, "trend"]
    p_val <- round(seaken_temp[, "p_value"], digits=3)
    x_delta <- as.numeric((xmax-xmin)/2)
    y_median <- median(data$Result_cen, na.rm = TRUE)
    sk_min <- y_median - x_delta*slope/365.25
    sk_max <- y_median + x_delta*slope/365.25
  }

  p <- ggplot2::ggplot(data)

  # add TMDL temperature Target lines
  if(any(!is.na(data$target_value))){
    target_periods <- unique(data[!is.na(data$start_datetime),c("start_datetime", "end_datetime")])
    for(i in 1:NROW(target_periods)){
      p <- p + ggplot2::geom_segment(aes(x=target_periods$start_datetime[i], xend=target_periods$end_datetime[i],
                                         y=target_value, yend=target_value,
                                         color = "TMDL Target", linetype = "TMDL Target", shape = "TMDL Target"))
    }
  }

  title <- paste(station, unique(data$StationDes))
  subtitle <- paste0("Assessment Unit: ", unique(data$AU_ID), " ", unique(data$AU_Name))

  # plot data with excursion colors
  p <- p + ggplot2::geom_point(aes(x=sample_datetime, y=Result_cen, color = excursion, linetype = excursion, shape = excursion)) +
    ggplot2::ggtitle(title, subtitle = subtitle) +
    ggplot2::ylab("Temperature (C)") +
    ggplot2::xlab("Datetime")

  # plot the trend line if applicable
  if(nrow(seaken_temp) > 0){
    p <- p + ggplot2::geom_segment(aes(x=xmin, xend=xmax, y=sk_min, yend=sk_max, color = "Trend", linetype = "Trend", shape = "Trend"), lwd = 1) +
      ggplot2::annotate("text", x = xmin, y = ymax, label = paste0("Trend Results: ", trend, ",  Z-Stat: ", p_val, ",  Slope: ", slope), hjust = 0, vjust = 0)
  }

  # apply color, shape, line types, and range limits
  p <- p +
    ggplot2::scale_color_manual(name = "",
                                values =    c('Excursion' = 'red', 'Result' = 'black', "Trend" = 'blue', "TMDL Target" = 'black')) +
    ggplot2::scale_linetype_manual(name = "",
                                   values = c('Excursion' = 0, 'Result' = 0, "Trend" = 2, "TMDL Target" = 1)) +
    ggplot2::scale_shape_manual(name = "",
                                values =    c('Excursion' = 4, 'Result' = 16, "Trend" = 32, "TMDL Target" = 32)) +
    ggplot2::ylim(c(ymin, ymax)) +
    ggplot2::xlim(c(xmin, xmax)) +
    ggplot2::scale_x_datetime(date_labels = "%b-%Y") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")

  return(p)
}
