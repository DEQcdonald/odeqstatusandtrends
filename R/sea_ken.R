#' Run Seasonal Kendall trend analysis for all stations with sufficient data
#'
#' Run the Season Kendall trend analysis from the EnvStats package. See ??EnvStats::kendallSeasonalTrendTest() for method.
#' @param data Dataframe to conduct a Seasonal Kendall Trend Analysis.
#' @return Dataframe of stations with the results of the Seasonal Kendall Trend Analysis
#' @export
#' @examples
#' sea_ken(data = data.frame)

sea_ken <- function(data, seasonal_estimates = F){
  if(odeqstatusandtrends::AWQMS_Char_Names('TP') %in% unique(data$Char_Name)){
    if("tp_year" %in% colnames(data)){
      data$Month <- dplyr::if_else(is.na(data$tp_month), lubridate::month(data$sample_datetime, label = TRUE, abbr = TRUE), data$tp_month)
      data$Year <- dplyr::if_else(is.na(data$tp_year), lubridate::year(data$sample_datetime), data$tp_year)
    } else {
      data$Month <- lubridate::month(data$sample_datetime, label = TRUE, abbr = TRUE)
      data$Year <- lubridate::year(data$sample_datetime)
    }
  } else {
    data$Month <- lubridate::month(data$sample_datetime, label = TRUE, abbr = TRUE)
    data$Year <- lubridate::year(data$sample_datetime)
  }

  sea_ken_df <- data.frame()
  sample_size <- data.frame()
  if(seasonal_estimates){
    seasonal_est_df <- data.frame()
  }

  for(j in unique(data$Char_Name)){
    print(j)
    parm_stations <- unique(data[data$Char_Name == j,]$MLocID)
    count <- 1
    for(i in parm_stations){
      print(paste(i, "(", count, "of", length(parm_stations), ")"))
      # subData <- dplyr::filter(data, Char_Name == j)
      # if(j == "Dissolved oxygen (DO)"){
      #   subData <- dplyr::filter(subData, !Statistical_Base %in% c(""))
      # }
      subData_stn <- data %>% dplyr::filter(Char_Name == j, MLocID == i)
      if(j == "Dissolved oxygen (DO)"){
        subData_stn <- dplyr::filter(subData_stn, !Statistical_Base %in% c(""))
      }
      tryCatch({
        tmp_seaKen <- EnvStats::kendallSeasonalTrendTest(y = subData_stn$Result_cen, season = subData_stn$Month,
                                                         year = subData_stn$Year, ci.slope = FALSE)
        tmp_sample_size <- as.data.frame(dplyr::bind_rows(tmp_seaKen$sample.size))
        tmp_sample_size[, c("ID", "Char")] <- c(i, j)

        stn_seaKen <- data.frame(MLocID = i,
                                 Char_Name = j,
                                 p_value = tmp_seaKen$p.value[2],
                                 # confidence = tmp_seaKen$interval$conf.level,
                                 slope = tmp_seaKen$estimate[2],
                                 intercept = tmp_seaKen$estimate[3])

        sample_size <- dplyr::bind_rows(sample_size, tmp_sample_size)
        sea_ken_df <- dplyr::bind_rows(sea_ken_df, stn_seaKen)
        if(seasonal_estimates){
          tmp_seasonal_est <- as.data.frame(tmp_seaKen$seasonal.estimates)
          tmp_seasonal_est$ID <- i
          tmp_seasonal_est$Char <- j
          seasonal_est_df <- dplyr::bind_rows(seasonal_est_df, tmp_seasonal_est)
        }
      }, error = function(e){cat("ERROR :", conditionMessage(e),"\n")})
      count <- count + 1
    }
  }

  sea_ken_df$significance <- dplyr::if_else(sea_ken_df$p_value <= 0.2 & !is.na(sea_ken_df$p_value), "Significant", "No Significant Trend")

  sea_ken_df$trend <- dplyr::if_else(sea_ken_df$significance == "Significant",
                                     dplyr::if_else(sea_ken_df$Char_Name %in% c("Dissolved oxygen (DO)"),
                                                    dplyr::if_else(sea_ken_df$slope > 0, "Improving",
                                                                   dplyr::if_else(sea_ken_df$slope == 0, "Steady", "Degrading")
                                                    ),
                                                    dplyr::if_else(sea_ken_df$Char_Name %in% c("pH"),
                                                                   dplyr::if_else(sea_ken_df$slope == 0, "Steady", "Degrading"),
                                                                   dplyr::if_else(sea_ken_df$slope < 0, "Improving",
                                                                                  dplyr::if_else(sea_ken_df$slope == 0, "Steady", "Degrading")
                                                                   )
                                                    )
                                     ), "No Significant Trend"
  )

  # sea_ken_df$trend <-
  #   dplyr::if_else(sea_ken_df$slope > 0 & sea_ken_df$significance == "Significant", "Increasing",
  #           dplyr::if_else(sea_ken_df$slope < 0 & sea_ken_df$significance == "Significant", "Decreasing",
  #                   dplyr::if_else(sea_ken_df$slope == 0 & sea_ken_df$significance == "Significant", "Steady", "No Significant Trend")
  #           )
  #   )

  for (i in c('ID', 'Char', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Total')){
    col_add <- if(!i %in% colnames(sample_size)){
      sample_size[i] <- NA
    }
    if(seasonal_estimates){
      col_add <- if(!i %in% colnames(seasonal_est_df)){
        seasonal_est_df[i] <- NA
      }
    }
  }

  attr(sea_ken_df, "sample_size") <- sample_size[, c('ID', 'Char', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
                                                     'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Total')]
  if(seasonal_estimates){
    attr(seasonal_est_df, "seasonal_estimates") <- seasonal_est_df[, c('ID', 'Char', 'slope', 'tau', 'intercept')]
  }
  return(sea_ken_df)
}
