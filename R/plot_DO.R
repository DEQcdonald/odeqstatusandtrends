#' Create dissolved oxygen plots to display status and trend.
#'
#'
#' @param data Dataframe to determine status from. Must have 'excursion_cen' column generated.
#' @param seaKen Results of Seasonal Kendall Analysis
#' @param station The station to plot
#' @return dataframe of stations with sufficient data
#' @export
#' @examples
#' plot_DO(data = data.frame, seaKen, station)

plot_DO <- function(data, seaKen, station){

  station_desc <- unique(data$StationDes)

  # obtain data range limits for plotting
  result_max <- max(c(data$Result_cen, data$Do_crit_instant), na.rm = TRUE)
  xmin <- min(data$sample_datetime, na.rm = TRUE)
  xmax <- max(data$sample_datetime, na.rm = TRUE)
  ymin <- 0
  ymax <- ifelse(result_max > 20, result_max, 20)

  # obtain plotting values for trend line if applicable
  if(station %in% seaKen$MLocID){
    slope <- seaKen[seaKen$MLocID == station & seaKen$Char_Name == "Dissolved oxygen (DO)", "slope"]
    x_delta <- as.numeric((xmax-xmin)/2)
    y_median <- median(data$Result_cen, na.rm = TRUE)
    sk_min <- y_median - x_delta*slope/365.25
    sk_max <- y_median + x_delta*slope/365.25
  }

  get_legend <- function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }

  p_inst <- NULL
  p_7dadmin <- NULL
  p_30dadmean <- NULL
  p_7dadmean <- NULL
  p_min <- NULL
  DO_plots <- list()

  p <- ggplot(data) +
    xlab(element_blank()) +
    scale_color_manual(name = "Legend",
                       values =    c('Excursion' = 'red', 'Result' = 'black', "Trend" = 'blue', "Spawning" = 'black', "Year-Round" = 'black')) +
    scale_linetype_manual(name = "Legend",
                          values = c('Excursion' = 0, 'Result' = 0, "Trend" = 2, "Spawning" = 1, "Year-Round" = 1)) +
    scale_shape_manual(name = "Legend",
                       values =    c('Excursion' = 16, 'Result' = 16, "Trend" = 32, "Spawning" = 32, "Year-Round" = 32)) +
    scale_fill_manual(name = "", values = c("Spawning Period" = 'black')) +
    ylim(c(ymin, ymax)) +
    ylab("DO (mg/L)") +
    theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal",
          panel.background = element_blank())

  # plot the trend line if applicable
  if(station %in% seaKen$MLocID){
    p <- p + geom_segment(aes(x=xmin, xend=xmax, y=sk_min, yend=sk_max, color = "Trend", linetype = "Trend"
                              , shape = "Trend"
                              ))
  }

  if(any(data$in_spawn == 1)){
    # Convert spawning dates to datetimes
    data$Start_spawn <- as.POSIXct(data$Start_spawn)
    data$End_spawn <- as.POSIXct(data$End_spawn)

    DO_inst <- data %>% dplyr::filter(is.na(Statistical_Base))
    DO_7DADMin <- data %>% dplyr::filter(Statistical_Base == "7DADMin")
    DO_30DADMean <- data %>% filter(Statistical_Base == "30DADMean")
    DO_7DADMean <- data %>% filter(Statistical_Base == "7DADMean")
    DO_Min <- data %>% filter(Statistical_Base == "Minimum")

    DO_inst$excursion <- if_else(((DO_inst$in_spawn == 1) & (DO_inst$spwn_exc_inst == 1)) | DO_inst$yr_exc_inst == 1, "Excursion", "Result") # change numeric value to descriptor
    DO_7DADMin$excursion <- if_else(DO_7DADMin$yr_exc_7DADMin == 1, "Excursion", "Result")
    DO_30DADMean$excursion <- if_else(DO_30DADMean$yr_exc_30DADMean == 1, "Excursion", "Result")
    DO_7DADMean$excursion <- if_else(DO_7DADMean$in_spawn == 1 & DO_7DADMean$spwn_exc_7DADMean == 1, "Excursion", "Result")
    DO_Min$excursion <- if_else(((DO_Min$in_spawn == 1) & (DO_Min$spwn_exc_min == 1)) | DO_Min$yr_exc_min == 1, "Excursion", "Result")

    # create dataframe of spawning start/end dates, and relevant values for Spawning Periods and criteria lines
    spawn_zones <- unique(data[,c("Start_spawn", "End_spawn", "Do_crit_30D", "Do_crit_7Mi", "DO_crit_min", "Do_crit_instant")])
    spawn_zones$next_start <- spawn_zones$Start_spawn + years(1)
    spawn_zones$y1 <- -Inf
    spawn_zones$y2 <- Inf
    # spawn_zones$Do_crit_instant <- unique(data$Do_crit_instant)
    spawn_zones$spawn_crit_inst <- 11
    # adjust plot limits to allow for first and last Spawning Periods to plot
    xmin <- min(xmin - lubridate::seconds(2), min(spawn_zones$Start_spawn, na.rm = TRUE) - lubridate::seconds(2))
    xmax <- max(xmax + lubridate::seconds(2), max(spawn_zones$End_spawn, na.rm = TRUE) + lubridate::seconds(2))

    # plot the shaded Spawning Periods
    p <- p + geom_rect(data = spawn_zones, aes(xmin=Start_spawn - lubridate::seconds(1), xmax= End_spawn + lubridate::seconds(1), ymin=ymin, ymax=ymax,
                                               # linetype = 'Spawning Period', shape = 'Spawning Period', color = 'Spawning Period',
                                               fill='Spawning Period'),
                       color = NA, alpha=.15, show.legend = c(fill=TRUE, linetype=FALSE, shape=FALSE, color=FALSE)) +
      scale_x_datetime(date_labels = "%b-%Y", limits = c(xmin - lubridate::seconds(1), xmax + lubridate::seconds(1)))

    if(nrow(DO_inst) > 0){
      # plot instantaneous Year-Round criteria lines within Year-Round periods
      p_inst <- p + geom_segment(data = spawn_zones,
                                 aes(x=xmin, xend=xmax, y=Do_crit_instant, yend=Do_crit_instant,
                                     color="Year-Round", linetype="Year-Round"
                                     , shape="Year-Round"
                                 ),
                                 size = 1)

      # plot instantaneous spawning criteria lines within Spawning Periods
      p_inst <- p_inst + geom_segment(data = spawn_zones,
                                      aes(x=Start_spawn - lubridate::seconds(1), xend=End_spawn + lubridate::seconds(1), y = 11, yend = 11,
                                          color="Spawning", linetype="Spawning"
                                          , shape="Spawning"
                                      ),
                                      size = 1)
    }

    if(nrow(DO_7DADMin) > 0){
      # plot 7DADmin year round criteria line
      p_7dadmin <- p + geom_segment(aes(x=xmin, xend=xmax, y=Do_crit_7Mi, yend=Do_crit_7Mi,
                                        color="Year-Round", linetype="Year-Round"
                                        , shape="Year-Round"
      ),
      size = 1)
    }

    if(nrow(DO_30DADMean) > 0){
      # plot 30DADmean year round criteria line
      p_30dadmean <- p + geom_segment(aes(x=xmin, xend=xmax, y=Do_crit_30D, yend=Do_crit_30D,
                                          color="Year-Round", linetype="Year-Round"
                                          , shape="Year-Round"
      ),
      size = 1)
    }

    if(nrow(DO_7DADMean) > 0){
      # plot 7DADmean spawning criteria lines within Spawning Periods
      p_7dadmean <- p + geom_segment(data = spawn_zones,
                                     aes(x=Start_spawn - lubridate::seconds(1), xend=End_spawn + lubridate::seconds(1), y = 11, yend = 11,
                                         color="Spawning", linetype="Spawning"
                                         , shape="Spawning"
                                     ),
                                     size = 1)
    }

    if(nrow(DO_Min) > 0){
      # plot DO min Year-Round criteria lines within Year-Round periods
      p_min <- p + geom_segment(data = spawn_zones,
                                aes(x=xmin, xend=xmax, y=DO_crit_min, yend=DO_crit_min,
                                    color="Year-Round", linetype="Year-Round"
                                    , shape="Year-Round"
                                ),
                                size = 1)

      # plot DO min spawning criteria lines within Spawning Periods
      p_min <- p_min + geom_segment(data = spawn_zones,
                                    aes(x=Start_spawn - lubridate::seconds(1), xend=End_spawn + lubridate::seconds(1), y = 11, yend = 11,
                                        color="Spawning", linetype="Spawning"
                                        , shape="Spawning"
                                    ),
                                    size = 1)
    }

  } else {

    DO_inst <- data %>% dplyr::filter(is.na(Statistical_Base))
    DO_7DADMin <- data %>% dplyr::filter(Statistical_Base == "7DADMin")
    DO_30DADMean <- data %>% filter(Statistical_Base == "30DADMean")
    DO_7DADMean <- data %>% filter(Statistical_Base == "7DADMean")
    DO_Min <- data %>% filter(Statistical_Base == "Minimum")

    DO_inst$excursion <- if_else(((DO_inst$in_spawn == 1) & (DO_inst$spwn_exc_inst == 1)) | DO_inst$yr_exc_inst == 1, "Excursion", "Result") # change numeric value to descriptor
    DO_7DADMin$excursion <- if_else(DO_7DADMin$yr_exc_7DADMin == 1, "Excursion", "Result")
    DO_30DADMean$excursion <- if_else(DO_30DADMean$yr_exc_30DADMean == 1, "Excursion", "Result")
    DO_7DADMean$excursion <- if_else(DO_7DADMean$in_spawn == 1 & DO_7DADMean$spwn_exc_7DADMean == 1, "Excursion", "Result")
    DO_Min$excursion <- if_else(((DO_Min$in_spawn == 1) & (DO_Min$spwn_exc_min == 1)) | DO_Min$yr_exc_min == 1, "Excursion", "Result")

    p <- p + scale_x_datetime(date_labels = "%b-%Y", limits = c(xmin - lubridate::seconds(1), xmax + lubridate::seconds(1)))

    if(nrow(DO_inst) > 0){
      # plot instantaneous Year-Round criteria lines within Year-Round periods
      p_inst <- p + geom_segment(aes(x=min(sample_datetime) - lubridate::seconds(1), xend=max(sample_datetime) + lubridate::seconds(1), y=Do_crit_instant, yend=Do_crit_instant,
                                     color="Year-Round", linetype="Year-Round"
                                     , shape="Year-Round"
                                 ),
                                 size = 1)

      # plot instantaneous spawning criteria lines within Spawning Periods
      # p_inst <- p_inst + geom_segment(aes(x=min(sample_datetime) - lubridate::seconds(1), xend=max(sample_datetime) + lubridate::seconds(1), y = 11, yend = 11,
      #                                     color="Spawning", linetype="Spawning"
      #                                     , shape="Spawning"
      #                                 ),
      #                                 size = 1)
    }

    if(nrow(DO_7DADMin) > 0){
      # plot 7DADmin year round criteria line
      p_7dadmin <- p + geom_segment(aes(x=min(sample_datetime) - lubridate::seconds(1), xend=max(sample_datetime) + lubridate::seconds(1), y=Do_crit_7Mi, yend=Do_crit_7Mi,
                                        color="Year-Round", linetype="Year-Round"
                                        , shape="Year-Round"
      ),
      size = 1)
    }

    if(nrow(DO_30DADMean) > 0){
      # plot 30DADmean year round criteria line
      p_30dadmean <- p + geom_segment(aes(x=min(sample_datetime) - lubridate::seconds(1), xend=max(sample_datetime) + lubridate::seconds(1), y=Do_crit_30D, yend=Do_crit_30D,
                                          color="Year-Round", linetype="Year-Round"
                                          , shape="Year-Round"
      ),
      size = 1)
    }

    if(nrow(DO_Min) > 0){
      # plot DO min spawning criteria lines within Spawning Periods
      p_min <- p + geom_segment(aes(x=min(sample_datetime) - lubridate::seconds(1), xend=max(sample_datetime) + lubridate::seconds(1), y=DO_crit_min, yend=DO_crit_min,
                                    color="Year-Round", linetype="Year-Round"
                                    , shape="Year-Round"),
                                size = 1)
    }

  }

  # plot data with excursion colors
  if(!is.null(p_inst)){
    p_inst <- p_inst + geom_point(data = DO_inst, aes(x=sample_datetime, y=Result_cen, color = excursion, shape = excursion
                                                      , linetype = excursion
    )) +
      # geom_point(aes(x=sample_datetime, y=DO_sat, linetype = excursion), color = "black", shape = 6) +
      ggtitle(paste(station, 'Instantaneous Dissolved Oxygen')
              , subtitle = paste(unique(data$StationDes))
              )

    legend <- get_legend(p_inst)

    # p_inst <- p_inst + theme(legend.position = "none")

    DO_plots[["instantaneous"]] <- p_inst
  }

  if(!is.null(p_7dadmin)){
    p_7dadmin <- p_7dadmin + geom_point(data = DO_7DADMin, aes(x=sample_datetime, y=Result_cen, color = excursion, shape = excursion
                                                               , linetype = excursion
    )) +
      ggtitle(paste(station, '7DADMin Dissolved Oxygen')
              , subtitle = paste(unique(data$StationDes))
              )
    # +
      # theme(legend.position = "none")

    DO_plots[["sdadmin"]] <- p_7dadmin
  }

  if(!is.null(p_30dadmean)){
    p_30dadmean <- p_30dadmean + geom_point(data = DO_30DADMean, aes(x=sample_datetime, y=Result_cen, color = excursion, shape = excursion
                                                                     , linetype = excursion
    )) +
      ggtitle(paste(station, '30DADMean Dissolved Oxygen')
              , subtitle = paste(unique(data$StationDes))
              )
    # +
      # theme(legend.position = "none")

    DO_plots[["30dadmean"]] <- p_30dadmean
  }

  if(!is.null(p_7dadmean)){
    p_7dadmean <- p_7dadmean + geom_point(data = DO_7DADMean, aes(x=sample_datetime, y=Result_cen, color = excursion, shape = excursion
                                                                  , linetype = excursion
    )) +
      ggtitle(paste(station, '7DADMean Dissolved Oxygen')
              , subtitle = paste(unique(data$StationDes))
              )
    # +
      # theme(legend.position = "none")

    DO_plots[["sdadmean"]] <- p_7dadmean
  }

  if(!is.null(p_min)){
    p_min <- p_min + geom_point(data = DO_Min, aes(x=sample_datetime, y=Result_cen, color = excursion, shape = excursion
                                                   , linetype = excursion
    )) +
      ggtitle(paste(station, 'Daily Minimum Dissolved Oxygen')
              , subtitle = paste(unique(data$StationDes))
              )
    # +
      # theme(legend.position = "none")

    DO_plots[["minimum"]] <- p_min
  }

  # title <- paste(station_desc, "Dissolved Oxygen")
  # subtitle <- paste("ID:", station)
  # grob1 <- textGrob(title, gp=gpar(fontface="bold", fontsize = 20))
  # grob2 <- textGrob(subtitle, gp=gpar(fontsize = 16))
  # titlegrob <- arrangeGrob(grob1, grob2, ncol = 1, nrow = 2)
  # DO_plots[["legend"]] <- legend

  # do_plot_grid <- grid.arrange(do.call("grid.arrange", c(DO_plots, ncol=1)), top = grob1, bottom = legend)

  # apply color, shape, line types, and range limits
  # p <- p +
  #   scale_color_manual(name = "Legend",
  #                      values =    c('Excursion' = 'red', 'Result' = 'black', "Trend" = 'blue', "Spawning" = 'black', "Year-Round" = 'black')) +
  #   scale_linetype_manual(name = "Legend",
  #                         values = c('Excursion' = 0, 'Result' = 0, "Trend" = 1, "Spawning" = 2, "Year-Round" = 1)) +
  #   scale_shape_manual(name = "Legend",
  #                      values =    c('Excursion' = 16, 'Result' = 16, "Trend" = 32, "Spawning" = 32, "Year-Round" = 32)) +
  #   scale_fill_manual(name = "", values = c("Spawning Period" = 'black')) +
  #   ylim(c(ymin, ymax)) +
  #   xlim(c(xmin - lubridate::seconds(1), xmax + lubridate::seconds(1))) +
  #   scale_x_datetime(date_labels = "%b-%Y")+
  #   theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")

  return(DO_plots)
}
