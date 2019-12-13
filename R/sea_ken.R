#' Run Seasonal Kendall trend analysis for all stations with sufficient data
#'
#' Run the Season Kendall trend analysis from the EnvStats package. See ??EnvStats::kendallSeasonalTrendTest() for method.
#' @param data Dataframe to conduct a Seasonal Kendall Trend Analysis.
#' @return Dataframe of stations with the results of the Seasonal Kendall Trend Analysis
#' @export
#' @examples
#' sea_ken(data = data.frame)

sea_ken <- function(data){
  if("Phosphate-phosphorus" %in% unique(data$Char_Name)){
    if("tp_year" %in% colnames(data)){
      data$Month <- if_else(is.na(data$tp_month), lubridate::month(data$sample_datetime, label = TRUE, abbr = TRUE), data$tp_month)
      data$Year <- if_else(is.na(data$tp_year), lubridate::year(data$sample_datetime), data$tp_year)
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

  for(j in unique(data$Char_Name)){
    print(j)
    parm_stations <- unique(data[data$Char_Name == j,]$MLocID)
    count <- 1
    for(i in parm_stations){
      print(paste(i, "(", count, "of", length(parm_stations), ")"))
      # subData <- filter(data, Char_Name == j)
      # if(j == "Dissolved oxygen (DO)"){
      #   subData <- filter(subData, !Statistical_Base %in% c(""))
      # }
      subData_stn <- data %>% filter(Char_Name == j, MLocID == i)
      if(j == "Dissolved oxygen (DO)"){
        subData_stn <- filter(subData_stn, !Statistical_Base %in% c(""))
      }
      tryCatch({
        tmp_seaKen <- EnvStats::kendallSeasonalTrendTest(y = subData_stn$Result_cen, season = subData_stn$Month,
                                                         year = subData_stn$Year, ci.slope = FALSE)
        tmp_sample_size <- as.data.frame(bind_rows(tmp_seaKen$sample.size))
        tmp_sample_size[, c("ID", "Char")] <- c(i, j)
        stn_seaKen <- data.frame(MLocID = i,
                                 Char_Name = j,
                                 p_value = tmp_seaKen$p.value[1],
                                 # confidence = tmp_seaKen$interval$conf.level,
                                 slope = tmp_seaKen$estimate[2],
                                 intercept = tmp_seaKen$estimate[3])

        sample_size <- bind_rows(sample_size, tmp_sample_size)
        sea_ken_df <- bind_rows(sea_ken_df, stn_seaKen)
        }, error = function(e){cat("ERROR :", conditionMessage(e),"\n")})
      count <- count + 1
    }
  }

  sea_ken_df$significance <- if_else(sea_ken_df$p_value <= .05 & !is.na(sea_ken_df$p_value), "Significant", "No Significant Trend")

  sea_ken_df$trend <- if_else(sea_ken_df$significance == "Significant",
                              if_else(sea_ken_df$Char_Name %in% c("Dissolved oxygen (DO)"),
                                      if_else(sea_ken_df$slope > 0, "Improving",
                                              if_else(sea_ken_df$slope == 0, "Steady", "Degrading")
                                      ),
                                      if_else(sea_ken_df$Char_Name %in% c("pH"),
                                              if_else(sea_ken_df$slope == 0, "Steady", "Degrading"),
                                              if_else(sea_ken_df$slope < 0, "Improving",
                                                      if_else(sea_ken_df$slope == 0, "Steady", "Degrading")
                                              )
                                      )
                              ), "No Significant Trend"
  )

  # sea_ken_df$trend <-
  #   if_else(sea_ken_df$slope > 0 & sea_ken_df$significance == "Significant", "Increasing",
  #           if_else(sea_ken_df$slope < 0 & sea_ken_df$significance == "Significant", "Decreasing",
  #                   if_else(sea_ken_df$slope == 0 & sea_ken_df$significance == "Significant", "Steady", "No Significant Trend")
  #           )
  #   )

  for (i in c('ID', 'Char', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Total')){
    col_add <- if(!i %in% colnames(sample_size)){
      sample_size[i] <- NA
    }
  }

  attr(sea_ken_df, "sample_size") <- sample_size[, c('ID', 'Char', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
                                                     'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Total')]
  return(sea_ken_df)
}
