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
    slope <- seaKen[seaKen$MLocID == station & seaKen$Char_Name == "Temperature, water", "slope"]
    x_min <- min(data$sample_datetime, na.rm = TRUE)
    x_max <- max(data$sample_datetime, na.rm = TRUE)
    x_delta <- as.numeric((x_max-x_min)/2)
    y_median <- median(data$Result_Numeric, na.rm = TRUE)
    sk_min <- y_median - x_delta*slope/365.25
    sk_max <- y_median + x_delta*slope/365.25
  }

  ymin <- min(c(data$Result_Numeric, data$temp_crit), na.rm = TRUE)
  ymax <- max(c(data$Result_Numeric, data$temp_crit), na.rm = TRUE)
  data$excursion <- as.character(data$excursion)

  p <- ggplot(data) +
    geom_point(aes(x=sample_datetime, y=Result_Numeric, color = excursion, linetype = excursion, shape = excursion)) +
    ggtitle(paste(station)) +
    ylab("Temperature (degrees C)") +
    xlab("Datetime")
  if(station %in% seaKen$MLocID){
    p <- p + geom_segment(aes(x=x_min, xend=x_max, y=sk_min, yend=sk_max, color = "Trend", linetype = "Trend", shape = "Trend"))
  }
  if(any(!is.na(data$temp_crit))){
    p <- p + geom_line(aes(x=sample_datetime, y=temp_crit, color=Spawn_type, linetype=Spawn_type, shape=Spawn_type))
  }
  if(any(data$Spawn_type == "Spawn")){
    p <- p + geom_line(aes(x=sample_datetime, y=spawning_crit, color=Spawn_type, linetype=Spawn_type, shape=Spawn_type))
  }

  if(any(data$Spawn_type == "Spawning")){
    spawn_start <- unique(data$spawn_start)
    spawn_end <- unique(data$spawn_end)
    years <- unique(lubridate::year(data$sample_datetime))

    rects <- data.frame(x1=as.POSIXct(paste0(years, "-", lubridate::month(spawn_start), "-", lubridate::day(spawn_start))),
                        x2=as.POSIXct(paste0(years, "-", lubridate::month(spawn_end), "-", lubridate::day(spawn_end))),
                        y1=-Inf, y2=Inf)
    p + geom_rect(data = rects, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color = NA, alpha=.4, fill='grey')
  }

  p <- p +
    scale_color_manual(values = c('1' = 'red', '0' = 'black', "Trend" = 'blue', "Spawn" = 'black', "Not_Spawn" = 'black')) +
    scale_linetype_manual(values = c('1' = 0, '0' = 0, "Trend" = 1, "Spawn" = 2, "Not_Spawn" = 3)) +
    scale_shape_manual(values = c('1' = 16, '0' = 16, "Trend" = 32, "Spawn" = 32, "Not_Spawn" = 32)) +
    ylim(c(ymin, ymax))

  return(p)
}
