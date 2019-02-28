#' Create temperature plots to display status and trend.
#'
#'
#' @param data Dataframe to determine status from. Must have 'exceed' column generated.
#' @param seaKen Results of Seasonal Kendall Analysis
#' @return dataframe of stations with sufficient data
#' @export
#' @example
#' plot_temperature(data = data.frame, seaKen, station)

plot_temperature <- function(data, seaKen, station){
  if(station %in% seaKen$MLocID){
    slope <- seaKen[seaKen$MLocID == station, "slope"]
    x_min <- min(plot_data$sample_datetime, na.rm = TRUE)
    x_max <- max(plot_data$sample_datetime, na.rm = TRUE)
    x_delta <- as.numeric((x_max-x_min)/2)
    y_median <- median(plot_data$Result_Numeric, na.rm = TRUE)
    sk_min <- y_median - x_delta*slope/365.25
    sk_max <- y_median + x_delta*slope/365.25
  }    
  
  p <- ggplot(plot_data) +
    geom_point(aes(x=sample_datetime, y=Result_Numeric, color=exceed, linetype = exceed, shape=exceed))
  if(station %in% seaKen$MLocID){
    p <- p + geom_segment(aes(x=x_min, xend=x_max, y=sk_min, yend=sk_max, color = "Trend", linetype = "Trend", shape = "Trend"))
  }
    
  p <- p + 
    scale_color_manual(values = c('TRUE' = 'red', 'FALSE' = 'black', "Trend" = 'blue')) +
    scale_linetype_manual(values = c('TRUE' = 0, 'FALSE' = 0, "Trend" = 1)) +
    scale_shape_manual(values = c('TRUE' = 16, 'FALSE' = 16, "Trend" = 20))
  
  return(p)
}