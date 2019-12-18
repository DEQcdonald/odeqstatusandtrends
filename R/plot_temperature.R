#' Create temperature plots to display status and trend.
#'
#'
#' @param data Dataframe to determine status from. Must have 'excursion' column generated.
#' @param seaKen Results of Seasonal Kendall Analysis
#' @param station The station to plot
#' @return dataframe of stations with sufficient data
#' @export
#' @examples
#' plot_temperature(data = data.frame, seaKen, station)

plot_temperature <- function(data, seaKen, station){
  # obtain data range limits for plotting
  result_max <- max(c(data$Result_cen, data$temp_crit), na.rm = TRUE)
  xmin <- min(data$sample_datetime, na.rm = TRUE)
  xmax <- max(data$sample_datetime, na.rm = TRUE)
  ymin <- 0
  ymax <- ifelse(result_max > 26, result_max, 26)

  data$excursion <- if_else(data$excursion_cen == 1, "Excursion", "Result") # change numeric value to descriptor
  if(all(is.na(data$excursion))){
    data$excursion <- "Result"
  }

  # obtain plotting values for trend line if applicable
  if(station %in% seaKen$MLocID){
    slope <- seaKen[seaKen$MLocID == station & seaKen$Char_Name == "Temperature, water", "slope"]
    x_delta <- as.numeric((xmax-xmin)/2)
    y_median <- median(data$Result_cen, na.rm = TRUE)
    sk_min <- y_median - x_delta*slope/365.25
    sk_max <- y_median + x_delta*slope/365.25
  }

  p <- ggplot(data)

  if(any(data$Spawn_type == "Spawn")){
    # Convert spawning dates to datetimes
    data$Start_spawn <- as.POSIXct(data$Start_spawn)
    data$End_spawn <- as.POSIXct(data$End_spawn)

    # create dataframe of spawning start/end dates, and relevant values for spawning period and criteria lines
    spawn_zones <- unique(data[,c("Start_spawn", "End_spawn")])
    spawn_zones$next_start <- spawn_zones$Start_spawn + years(1)
    spawn_zones$y1 <- -Inf
    spawn_zones$y2 <- Inf
    spawn_zones$temp_crit <- unique(data$temp_crit)
    spawn_zones$spawn_crit <- 13
    # adjust plot limits to allow for first and last spawning period to plot
    xmin <- min(xmin, min(spawn_zones$Start_spawn, na.rm = TRUE))
    xmax <- max(xmax, max(spawn_zones$End_spawn, na.rm = TRUE))

    # plot the shaded spawning period
    p <- p + geom_rect(data = spawn_zones, aes(xmin=Start_spawn, xmax=End_spawn, ymin=ymin, ymax=ymax,
                                               # linetype = 'Spawning Zone', shape = 'Spawning Zone', color = 'Spawning Zone',
                                               fill='Spawning Period'),
                       color = NA, alpha=.2, show.legend = c(fill=TRUE, linetype=FALSE, shape=FALSE, color=FALSE))

    # plot non-spawning criteria lines within non-spawning period
    p <- p + geom_segment(data = spawn_zones,
                          aes(x=End_spawn, xend=next_start, y=temp_crit, yend=temp_crit,
                              color="Non-Spawning", linetype="Non-Spawning", shape="Non-Spawning"),
                          size = 1)

    # plot spawning criteria lines within spawning period
    p <- p + geom_segment(data = spawn_zones,
                          aes(x=Start_spawn, xend=End_spawn, y=spawn_crit, yend=spawn_crit,
                              color="Spawning", linetype="Spawning", shape="Spawning"),
                          size = 1)

  } else if(any(!is.na(data$temp_crit))){
    # plot non-spawining line across data if no spawning period apply
    p <- p + geom_line(aes(x=sample_datetime, y=temp_crit, color="Non-Spawning", linetype="Non-Spawning", shape="Non-Spawning"))
  }

  # plot data with excursion colors
  p <- p + geom_point(aes(x=sample_datetime, y=Result_cen, color = excursion, linetype = excursion, shape = excursion)) +
    ggtitle(paste(station, 'Temperature'), subtitle = paste(unique(data$StationDes))) +
    ylab("7DADM Temperature (deg C)") +
    xlab("Datetime")

  # plot the trend line if applicable
  if(station %in% seaKen$MLocID){
    p <- p + geom_segment(aes(x=xmin, xend=xmax, y=sk_min, yend=sk_max, color = "Trend", linetype = "Trend", shape = "Trend"))
  }

  # apply color, shape, line types, and range limits
  p <- p +
    scale_color_manual(name = "",
                       values =    c('Excursion' = 'red', 'Result' = 'black', "Trend" = 'blue', "Spawning" = 'black', "Non-Spawning" = 'black')) +
    scale_linetype_manual(name = "",
                          values = c('Excursion' = 0, 'Result' = 0, "Trend" = 2, "Spawning" = 1, "Non-Spawning" = 1)) +
    scale_shape_manual(name = "",
                       values =    c('Excursion' = 16, 'Result' = 16, "Trend" = 32, "Spawning" = 32, "Non-Spawning" = 32)) +
    scale_fill_manual(name = "", values = c("Spawning Period" = 'black')) +
    ylim(c(ymin, ymax)) +
    xlim(c(xmin, xmax)) +
    scale_x_datetime(date_labels = "%b-%Y")+
    theme_bw() +
    theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")

  return(p)
}
