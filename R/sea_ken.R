#' Run Seasonal Kendall trend analysis for all stations with sufficient data
#'
#' Run the Season Kendall trend analysis from the EnvStats package. See ??EnvStats::kendallSeasonalTrendTest() for method.
#' @param data Dataframe to conduct a Seasonal Kendall Trend Analysis.
#' @return Dataframe of stations with the results of the Seasonal Kendall Trend Analysis
#' @export
#' @examples
#' sea_ken(data = data.frame)

sea_ken <- function(data){
  data$Month <- lubridate::month(data$sample_datetime, label = TRUE, abbr = TRUE)
  data$Year <- lubridate::year(data$sample_datetime)

  sea_ken_df <- data.frame()
  sample_size <- data.frame()

  for(j in unique(data$Char_Name)){
    print(j)
    subData <- filter(data, Char_Name == j)
    for(i in unique(subData$MLocID)){
      print(i)
      subData_stn <- subData %>% filter(MLocID == i)
      tryCatch({
        tmp_seaKen <- EnvStats::kendallSeasonalTrendTest(Result_Numeric ~ Month + Year, data = subData_stn)
        tmp_sample_size <- as.data.frame(bind_rows(tmp_seaKen$sample.size))
        tmp_sample_size[, c("ID", "Char")] <- c(i, j)
        stn_seaKen <- data.frame(MLocID = i,
                                 Char_Name = j,
                                 p_value = tmp_seaKen$p.value[1],
                                 confidence = tmp_seaKen$interval$conf.level,
                                 slope = tmp_seaKen$estimate[2],
                                 intercept = tmp_seaKen$estimate[3])

        sample_size <- bind_rows(sample_size, tmp_sample_size)
        sea_ken_df <- bind_rows(sea_ken_df, stn_seaKen)
        }, error = function(e){cat("ERROR :", conditionMessage(e),"\n")})
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

  attr(sea_ken_df, "sample_size") <- sample_size[, c('ID', 'Char', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
                                                     'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Total')]
  return(sea_ken_df)
}
