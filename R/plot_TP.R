#' Create Total Phosphorus plots to display status and trend.
#'
#'
#' @param data Dataframe to determine status from. Must have 'excursion' column generated.
#' @param seaKen Results of Seasonal Kendall Analysis
#' @param station The station to plot
#' @return dataframe of stations with sufficient data
#' @export
#' @examples
#' plot_pH(data = data.frame, seaKen, station)

plot_TP <- function(data, seaKen, station){
  # subset seaken table to parameter and significant trends
  seaken_TP <- seaKen %>% filter(Char_Name == odeqstatusandtrends::AWQMS_Char_Names('TP'),
                                 significance != "No Significant Trend",
                                 MLocID == station)

  # obtain data range limits for plotting
  result_max <- max(c(data$Result_cen, data$TP_crit), na.rm = TRUE)
  xmin <- min(data$sample_datetime, na.rm = TRUE)
  xmax <- max(data$sample_datetime, na.rm = TRUE)
  ymin <- 0
  ymax <- ifelse(result_max > 0.15, result_max, 0.15)
  data$excursion <- if_else(!is.na(data$excursion_cen),
                            if_else(data$excursion_cen == 1, "Excursion", "Result"),
                            "Result") # change numeric value to descriptor

  # obtain plotting values for trend line if applicable
  if(nrow(seaken_TP) > 0){
    slope <- round(seaken_TP[, "slope"], digits=3)
    trend <- seaken_TP[, "trend"]
    p_val <- round(seaken_TP[, "p_value"], digits=3)
    x_delta <- as.numeric((xmax-xmin)/2)
    y_median <- median(data$Result_cen, na.rm = TRUE)
    sk_min <- y_median - x_delta*slope/365.25
    sk_max <- y_median + x_delta*slope/365.25
  }

  p <- ggplot(data)

  # add TMDL TP Target line
  if(any(!is.na(data$TP_crit))){
    p <- p + geom_segment(aes(x=xmin, xend=xmax, y=TP_crit, yend=TP_crit,
                              color = "TMDL Target", linetype = "TMDL Target", shape = "TMDL Target"))
  }

  title <- paste(station, unique(data$StationDes))
  subtitle <- paste0("Assessment Unit: ", unique(data$AU_ID), " ", unique(data$AU_Name))

  # plot data with excursion colors
  p <- p + geom_point(aes(x=sample_datetime, y=Result_cen, color = excursion, linetype = excursion, shape = excursion)) +
    ggtitle(title, subtitle = subtitle) +
    ylab("Total Phosphorus (mg/L)") +
    xlab("Datetime")

  # plot the trend line if applicable
  if(nrow(seaken_TP) > 0){
    p <- p + geom_segment(aes(x=xmin, xend=xmax, y=sk_min, yend=sk_max, color = "Trend", linetype = "Trend", shape = "Trend"), lwd = 1) +
      annotate("text", x = xmin, y = ymax, label = paste0("Trend Results: ", trend, ",  Z-Stat: ", p_val, ",  Slope: ", slope), hjust = 0, vjust = 0)
  }

  # apply color, shape, line types, and range limits
  p <- p +
    scale_color_manual(name = "",
                       values =    c('Excursion' = 'red', 'Result' = 'black', "Trend" = 'blue', "TMDL Target" = 'black')) +
    scale_linetype_manual(name = "",
                          values = c('Excursion' = 0, 'Result' = 0, "Trend" = 2, "TMDL Target" = 1)) +
    scale_shape_manual(name = "",
                       values =    c('Excursion' = 4, 'Result' = 16, "Trend" = 32, "TMDL Target" = 32)) +
    ylim(c(ymin, ymax)) +
    xlim(c(xmin, xmax)) +
    scale_x_datetime(date_labels = "%b-%Y")+
    theme_bw() +
    theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")

  return(p)
}
