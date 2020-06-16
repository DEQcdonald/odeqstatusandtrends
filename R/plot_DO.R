#' Create dissolved oxygen plots to display status and trend.
#'
#'
#' @param data Dataframe to determine status from. Must have 'excursion_cen' column generated.
#' @param seaKen Results of Seasonal Kendall Analysis
#' @param station The station to plot
#' @param max_date The max date to show on the plot
#' @return dataframe of stations with sufficient data
#' @export
#' @examples
#' plot_DO(data = data.frame, seaKen, station)

plot_DO <- function(data, seaKen, station, max_date = min(data$sample_datetime, na.rm = TRUE)){

  station_desc <- unique(data$StationDes)

  # obtain data range limits for plotting
  result_max <- max(c(data$Result_cen, data$Do_crit_instant), na.rm = TRUE)
  xmin <- min(data$sample_datetime, na.rm = TRUE)
  xmax <- max_date
  ymin <- 0
  ymax <- ifelse(result_max > 20, result_max, 20)

  # obtain plotting values for trend line if applicable
  if(station %in% seaKen$MLocID){
    slope <- round(seaKen[seaKen$MLocID == station & seaKen$Char_Name == "Dissolved oxygen (DO)", "slope"], digits=3)
    trend <- seaKen[seaKen$MLocID == station & seaKen$Char_Name == "Dissolved oxygen (DO)", "trend"]
    p_val <- round(seaKen[seaKen$MLocID == station & seaKen$Char_Name == "Dissolved oxygen (DO)", "p_value"], digits=3)
    x_delta <- as.numeric((xmax-xmin)/2)
    y_median <- median(data$Result_cen, na.rm = TRUE)
    sk_min <- y_median - x_delta*slope/365.25
    sk_max <- y_median + x_delta*slope/365.25
  }

  get_legend <- function(myggplot){
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(myggplot))
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

  p <- ggplot2::ggplot(data) +
    ggplot2::scale_color_manual(name = "",
                                values =    c('Excursion' = 'red', 'Result' = 'black', "Trend" = 'blue', "Spawning" = 'black', "Year-Round" = 'black')) +
    ggplot2::scale_linetype_manual(name = "",
                                   values = c('Excursion' = 0, 'Result' = 0, "Trend" = 2, "Spawning" = 1, "Year-Round" = 1)) +
    ggplot2::scale_shape_manual(name = "",
                                values =    c('Excursion' = 4, 'Result' = 16, "Trend" = 32, "Spawning" = 32, "Year-Round" = 32)) +
    ggplot2::scale_fill_manual(name = "", values = c("Spawning Period" = 'black')) +
    ggplot2::ylim(c(ymin, ymax)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Datetime") +
    ggplot2::theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")

  if(any(data$Spawn_type == "Spawn", na.rm = TRUE)){
    # Convert spawning dates to datetimes
    data$Start_spawn <- as.POSIXct(data$Start_spawn)
    data$End_spawn <- as.POSIXct(data$End_spawn)

    DO_inst <- data %>% dplyr::filter(is.na(Statistical_Base))
    DO_7DADMin <- data %>% dplyr::filter(Statistical_Base == "7DADMin")
    DO_30DADMean <- data %>% dplyr::filter(Statistical_Base == "30DADMean")
    DO_7DADMean <- data %>% dplyr::filter(Statistical_Base == "7DADMean")
    DO_Min <- data %>% dplyr::filter(Statistical_Base == "Minimum")

    DO_inst$excursion <- dplyr::if_else(((DO_inst$Spawn_type == "Spawn") & (DO_inst$spwn_exc_inst == 1)) | DO_inst$yr_exc_inst == 1, "Excursion", "Result") # change numeric value to descriptor
    DO_inst$excursion <- dplyr::if_else(is.na(DO_inst$excursion), "Result", DO_inst$excursion)
    DO_7DADMin$excursion <- dplyr::if_else(DO_7DADMin$yr_exc_7DADMin == 1, "Excursion", "Result")
    DO_30DADMean$excursion <- dplyr::if_else(DO_30DADMean$yr_exc_30DADMean == 1, "Excursion", "Result")
    DO_7DADMean$excursion <- dplyr::if_else(DO_7DADMean$Spawn_type == "Spawn" & DO_7DADMean$spwn_exc_7DADMean == 1, "Excursion", "Result")
    DO_Min$excursion <- dplyr::if_else(((DO_Min$Spawn_type == "Spawn") & (DO_Min$spwn_exc_min == 1)) | DO_Min$yr_exc_min == 1, "Excursion", "Result")

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
    p <- p + ggplot2::geom_rect(data = spawn_zones, aes(xmin=Start_spawn - lubridate::seconds(1), xmax= End_spawn + lubridate::seconds(1), ymin=ymin, ymax=ymax,
                                                        # linetype = 'Spawning Period', shape = 'Spawning Period', color = 'Spawning Period',
                                                        fill='Spawning Period'),
                                color = NA, alpha=.15, show.legend = c(fill=TRUE, linetype=FALSE, shape=FALSE, color=FALSE)) +
      ggplot2::scale_x_datetime(date_labels = "%b-%Y", limits = c(xmin - lubridate::seconds(1), xmax + lubridate::seconds(1))
                                # , expand = expand_scale(mult = 0.01)
      )

    if(nrow(DO_inst) > 0){
      # plot instantaneous Year-Round criteria lines within Year-Round periods
      p_inst <- p + ggplot2::geom_segment(data = spawn_zones,
                                          aes(x=xmin, xend=xmax, y=Do_crit_instant, yend=Do_crit_instant,
                                              color="Year-Round", linetype="Year-Round"
                                              , shape="Year-Round"
                                          ),
                                          size = 1)

      # plot instantaneous spawning criteria lines within Spawning Periods
      p_inst <- p_inst + ggplot2::geom_segment(data = spawn_zones,
                                               aes(x=Start_spawn - lubridate::seconds(1), xend=End_spawn + lubridate::seconds(1), y = 11, yend = 11,
                                                   color="Spawning", linetype="Spawning"
                                                   , shape="Spawning"
                                               ),
                                               size = 1)
    }

    if(nrow(DO_7DADMin) > 0){
      # plot 7DADmin year round criteria line
      p_7dadmin <- p + ggplot2::geom_segment(aes(x=xmin, xend=xmax, y=Do_crit_7Mi, yend=Do_crit_7Mi,
                                                 color="Year-Round", linetype="Year-Round"
                                                 , shape="Year-Round"
      ),
      size = 1)
    }

    if(nrow(DO_30DADMean) > 0){
      # plot 30DADmean year round criteria line
      p_30dadmean <- p + ggplot2::geom_segment(aes(x=xmin, xend=xmax, y=Do_crit_30D, yend=Do_crit_30D,
                                                   color="Year-Round", linetype="Year-Round"
                                                   , shape="Year-Round"
      ),
      size = 1)
    }

    if(nrow(DO_7DADMean) > 0){
      # plot 7DADmean spawning criteria lines within Spawning Periods
      p_7dadmean <- p + ggplot2::geom_segment(data = spawn_zones,
                                              aes(x=Start_spawn - lubridate::seconds(1), xend=End_spawn + lubridate::seconds(1), y = 11, yend = 11,
                                                  color="Spawning", linetype="Spawning"
                                                  , shape="Spawning"
                                              ),
                                              size = 1)
    }

    if(nrow(DO_Min) > 0){
      # plot DO min Year-Round criteria lines within Year-Round periods
      p_min <- p + ggplot2::geom_segment(data = spawn_zones,
                                         aes(x=xmin, xend=xmax, y=DO_crit_min, yend=DO_crit_min,
                                             color="Year-Round", linetype="Year-Round"
                                             , shape="Year-Round"
                                         ),
                                         size = 1)

      # plot DO min spawning criteria lines within Spawning Periods
      p_min <- p_min + ggplot2::geom_segment(data = spawn_zones,
                                             aes(x=Start_spawn - lubridate::seconds(1), xend=End_spawn + lubridate::seconds(1), y = 11, yend = 11,
                                                 color="Spawning", linetype="Spawning"
                                                 , shape="Spawning"
                                             ),
                                             size = 1)
    }

  } else {

    DO_inst <- data %>% dplyr::filter(is.na(Statistical_Base))
    DO_7DADMin <- data %>% dplyr::filter(Statistical_Base == "7DADMin")
    DO_30DADMean <- data %>% dplyr::filter(Statistical_Base == "30DADMean")
    DO_7DADMean <- data %>% dplyr::filter(Statistical_Base == "7DADMean")
    DO_Min <- data %>% dplyr::filter(Statistical_Base == "Minimum")

    DO_inst$excursion <- dplyr::if_else(((DO_inst$Spawn_type == "Spawn") & (DO_inst$spwn_exc_inst == 1)) | DO_inst$yr_exc_inst == 1, "Excursion", "Result") # change numeric value to descriptor
    DO_7DADMin$excursion <- dplyr::if_else(DO_7DADMin$yr_exc_7DADMin == 1, "Excursion", "Result")
    DO_30DADMean$excursion <- dplyr::if_else(DO_30DADMean$yr_exc_30DADMean == 1, "Excursion", "Result")
    DO_7DADMean$excursion <- dplyr::if_else(DO_7DADMean$Spawn_type == "Spawn" & DO_7DADMean$spwn_exc_7DADMean == 1, "Excursion", "Result")
    DO_Min$excursion <- dplyr::if_else(((DO_Min$Spawn_type == "Spawn") & (DO_Min$spwn_exc_min == 1)) | DO_Min$yr_exc_min == 1, "Excursion", "Result")

    p <- p + ggplot2::scale_x_datetime(date_labels = "%b-%Y", limits = c(xmin - lubridate::seconds(1), xmax + lubridate::seconds(1))
                                       # , expand = expand_scale(mult = 0.01)
    )

    if(nrow(DO_inst) > 0){
      # plot instantaneous Year-Round criteria lines within Year-Round periods
      p_inst <- p + ggplot2::geom_segment(aes(x=min(sample_datetime) - lubridate::seconds(1), xend=max(sample_datetime) + lubridate::seconds(1), y=Do_crit_instant, yend=Do_crit_instant,
                                              color="Year-Round", linetype="Year-Round"
                                              , shape="Year-Round"
      ),
      size = 1)

      # plot instantaneous spawning criteria lines within Spawning Periods
      # p_inst <- p_inst + ggplot2::geom_segment(aes(x=min(sample_datetime) - lubridate::seconds(1), xend=max(sample_datetime) + lubridate::seconds(1), y = 11, yend = 11,
      #                                     color="Spawning", linetype="Spawning"
      #                                     , shape="Spawning"
      #                                 ),
      #                                 size = 1)
    }

    if(nrow(DO_7DADMin) > 0){
      # plot 7DADmin year round criteria line
      p_7dadmin <- p + ggplot2::geom_segment(aes(x=min(sample_datetime) - lubridate::seconds(1), xend=max(sample_datetime) + lubridate::seconds(1), y=Do_crit_7Mi, yend=Do_crit_7Mi,
                                                 color="Year-Round", linetype="Year-Round"
                                                 , shape="Year-Round"
      ),
      size = 1)
    }

    if(nrow(DO_30DADMean) > 0){
      # plot 30DADmean year round criteria line
      p_30dadmean <- p + ggplot2::geom_segment(aes(x=min(sample_datetime) - lubridate::seconds(1), xend=max(sample_datetime) + lubridate::seconds(1), y=Do_crit_30D, yend=Do_crit_30D,
                                                   color="Year-Round", linetype="Year-Round"
                                                   , shape="Year-Round"
      ),
      size = 1)
    }

    if(nrow(DO_Min) > 0){
      # plot DO min spawning criteria lines within Spawning Periods
      p_min <- p + ggplot2::geom_segment(aes(x=min(sample_datetime) - lubridate::seconds(1), xend=max(sample_datetime) + lubridate::seconds(1), y=DO_crit_min, yend=DO_crit_min,
                                             color="Year-Round", linetype="Year-Round"
                                             , shape="Year-Round"),
                                         size = 1)
    }

  }

  title <- paste(station, unique(data$StationDes))
  subtitle <- paste0("Assessment Unit: ", unique(data$AU_ID), " ", unique(data$AU_Name))

  # plot data with excursion colors
  if(!is.null(p_inst)){
    # plot the trend line if applicable
    if(station %in% seaKen$MLocID){
      p_inst <- p_inst + ggplot2::geom_segment(aes(x=xmin, xend=xmax, y=sk_min, yend=sk_max, color = "Trend", linetype = "Trend", shape = "Trend"), lwd = 1) +
        annotate("text", x = xmin, y = ymax, label = paste0("Trend Results: ", trend, ",  Z-Stat: ", p_val, ",  Slope: ", slope), hjust = 0, vjust = 0)
    }
    p_inst <- p_inst + ggplot2::geom_point(data = DO_inst, aes(x=sample_datetime, y=Result_cen, color = excursion, shape = excursion
                                                               , linetype = excursion
    )) +
      # ggplot2::geom_point(aes(x=sample_datetime, y=DO_sat, linetype = excursion), color = "black", shape = 6) +
      ggplot2::ggtitle(label = title, subtitle = subtitle) +
      ggplot2::ylab("Instantaneous Dissolved Oxygen (mg/L)")

    legend <- cowplot::get_legend(p_inst)

    # p_inst <- p_inst + theme(legend.position = "none")

    DO_plots[["instantaneous"]] <- p_inst
  }

  if(!is.null(p_7dadmin)){
    p_7dadmin <- p_7dadmin + ggplot2::geom_point(data = DO_7DADMin, aes(x=sample_datetime, y=Result_cen, color = excursion, shape = excursion
                                                                        , linetype = excursion
    )) +
      ggplot2::ggtitle(label = title, subtitle = subtitle) +
      ggplot2::ylab("7DADMin Dissolved Oxygen (mg/L)")
    # +
    # theme(legend.position = "none")

    DO_plots[["sdadmin"]] <- p_7dadmin
  }

  if(!is.null(p_30dadmean)){
    p_30dadmean <- p_30dadmean + ggplot2::geom_point(data = DO_30DADMean, aes(x=sample_datetime, y=Result_cen, color = excursion, shape = excursion
                                                                              , linetype = excursion
    )) +
      ggplot2::ggtitle(label = title, subtitle = subtitle) +
      ggplot2::ylab("30DADMean Dissolved Oxygen (mg/L)")
    # +
    # theme(legend.position = "none")

    DO_plots[["30dadmean"]] <- p_30dadmean
  }

  if(!is.null(p_7dadmean)){
    p_7dadmean <- p_7dadmean + ggplot2::geom_point(data = DO_7DADMean, aes(x=sample_datetime, y=Result_cen, color = excursion, shape = excursion
                                                                           , linetype = excursion
    )) +
      ggplot2::ggtitle(label = title, subtitle = subtitle) +
      ggplot2::ylab("7DADMean Dissolved Oxygen (mg/L)")
    # +
    # theme(legend.position = "none")

    DO_plots[["sdadmean"]] <- p_7dadmean
  }

  if(!is.null(p_min)){
    p_min <- p_min + ggplot2::geom_point(data = DO_Min, aes(x=sample_datetime, y=Result_cen, color = excursion, shape = excursion
                                                            , linetype = excursion
    )) +
      ggplot2::ggtitle(label = title, subtitle = subtitle) +
      ggplot2::ylab("Daily Minimum Dissolved Oxygen (mg/L)")
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
