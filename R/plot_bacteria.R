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
  parameter <- unique(data$Char_Name)
  # subset seaken table to parameter and significant trends
  seaken_bact <- seaKen %>% dplyr::filter(Char_Name %in% odeqstatusandtrends::AWQMS_Char_Names("bacteria"),
                                          significance != "No Significant Trend",
                                          MLocID == station)
  
  # obtain data range limits for plotting
  result_max <- max(c(data$Result_cen, data$bact_crit_ss, data$bact_crit_geomean), na.rm = TRUE)
  
  uylim <- dplyr::case_when(parameter== "Escherichia coli" ~ as.numeric(1000),
                            parameter== "Fecal Coliform" ~ as.numeric(600),
                            parameter== "Enterococcus" ~ as.numeric(100),
                            TRUE ~ as.numeric(100))
  
  xmin <- min(data$sample_datetime, na.rm = TRUE)
  xmax <- max(data$sample_datetime, na.rm = TRUE)
  ymin <- 0
  ymax <- ifelse(result_max > uylim, result_max, uylim)
  data$ss_excursion <- dplyr::if_else(data$ss_excursion == 1, "Excursion", "Result") # change numeric value to descriptor
  data$geomean_excursion <- dplyr::if_else(data$geomean_excursion == 1, "Excursion", "Result") # change numeric value to descriptor
  
  # obtain plotting values for trend line if applicable
  if(nrow(seaken_bact) > 0){
    slope <- round(seaken_bact[, "slope"], digits=3)
    trend <- seaken_bact[, "trend"]
    p_val <- round(seaken_bact[, "p_value"], digits=3)
    x_delta <- as.numeric((xmax-xmin)/2)
    y_median <- median(data$Result_cen, na.rm = TRUE)
    sk_min <- y_median - x_delta*slope/365.25
    sk_max <- y_median + x_delta*slope/365.25
  }
  
  p <- ggplot2::ggplot(data)
  
  title <- paste(station, unique(data$StationDes), "Single Sample")
  subtitle <- paste0("Assessment Unit: ", unique(data$AU_ID), " ", unique(data$AU_Name))
  
  # apply color, shape, line types, and range limits
  p <- p +
    ggplot2::scale_color_manual(name = "",
                                values =    c('Excursion' = 'red', 'Result' = 'black', "Trend" = 'blue',
                                              "Single Sample Criteria" = 'black', "Geomean Criteria" = 'black')) +
    ggplot2::scale_linetype_manual(name = "",
                                   values = c('Excursion' = 0, 'Result' = 0, "Trend" = 2,
                                              "Single Sample Criteria" = 1, "Geomean Criteria" = 6)) +
    ggplot2::scale_shape_manual(name = "",
                                values =    c('Excursion' = 4, 'Result' = 16, "Trend" = 32,
                                              "Single Sample Criteria" = 32, "Geomean Criteria" = 32)) +
    ggplot2::ylim(c(ymin, ymax)) +
    ggplot2::xlim(c(xmin, xmax)) +
    ggplot2::scale_x_datetime(date_labels = "%b-%Y")+
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")
  
  
  
  
  # plot single sample data with excursion colors
  p_ss <- p + geom_point(aes(x=sample_datetime, y=Result_cen, color = ss_excursion, linetype = ss_excursion, shape = ss_excursion)) +
    ggtitle(title, subtitle = subtitle) +
    ylab(paste0(parameter, "/100ml")) +
    xlab("Datetime")
  
  # plot the trend line if applicable
  if(nrow(seaken_bact) > 0){
    p_ss <- p_ss + geom_segment(aes(x=xmin, xend=xmax, y=sk_min, yend=sk_max, color = "Trend", linetype = "Trend", shape = "Trend"), lwd = 1) +
      annotate("text", x = xmin, y = ymax, label = paste0("Trend Results: ", trend, ",  Z-Stat: ", p_val, ",  Slope: ", slope), hjust = 0, vjust = 0)
  }
  
  # add ss criteria lines
  if(any(!is.na(data$bact_crit_ss))){
    p_ss <- p_ss + geom_segment(aes(x=xmin, xend=xmax, y=bact_crit_ss, yend=bact_crit_ss,
                                    color = "Single Sample Criteria", linetype = "Single Sample Criteria", shape = "Single Sample Criteria"))
  }
  
  bact_plots[["ss"]] <- p_ss
  
  # plot geomean data with excursion colors
  if(any(!is.na(data$geomean))){
    title_geo <- paste(station, unique(data$StationDes), "Geomean")
    
    p_geomean <- p + ggplot2::geom_point(aes(x=sample_datetime, y=geomean, color = geomean_excursion, linetype = geomean_excursion, shape = geomean_excursion)) +
      ggplot2::ggtitle(title_geo, subtitle = subtitle) +
      ggplot2::ylab(paste0(parameter, "/100ml")) +
      ggplot2::xlab("Datetime")
    
    if(any(!is.na(data$bact_crit_geomean))){
      p_geomean <- p_geomean + ggplot2::geom_segment(aes(x=xmin, xend=xmax, y=bact_crit_geomean, yend=bact_crit_geomean,
                                                         color = "Geomean Criteria", linetype = "Geomean Criteria", shape = "Geomean Criteria"))
    }
    
    bact_plots[["geomean"]] <- p_geomean
  }
  
  return(bact_plots)
}
