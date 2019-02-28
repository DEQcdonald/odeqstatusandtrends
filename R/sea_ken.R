#' Run Seasonal Kendall trend analysis for all stations with sufficient data
#'
#'
#' @param data Dataframe to conduct a Seasonal Kendall Trend Analysis.
#' @return dataframe of stations with the results of the Seasonal Kendall Trend Analysis
#' @export
#' @example
#' sea_ken(data = data.frame)

sea_ken <- function(data){
  data$Month <- lubridate::month(data$sample_datetime)
  data$Year <- lubridate::year(data$sample_datetime)

  sea_ken_df <- data.frame()
  sample_size <- data.frame()

  for(j in unique(data$Char_Name)){
    print(j)
    subData <- filter(data, Char_Name == j)
    for(i in unique(subData$MLocID)){
      print(i)
      subData_stn <- subData %>% filter(MLocID == i)
      tmp_seaKen <- EnvStats::kendallSeasonalTrendTest(Result_Numeric ~ Month + Year, data = subData_stn)
      tmp_sample_size <- tmp_seaKen$sample.size
      stn_seaKen <- data.frame(MLocID = i,
                               Char_Name = j,
                               p_value = tmp_seaKen$p.value[1],
                               confidence = tmp_seaKen$interval$conf.level,
                               slope = tmp_seaKen$estimate[2],
                               intercept = tmp_seaKen$estimate[3])

      sample_size <- bind_rows(sample_size, tmp_sample_size)
      sea_ken_df <- bind_rows(sea_ken_df, stn_seaKen)
    }
  }
  attr(sea_ken_df, "sample_size") <- sample_size
  return(sea_ken_df)
}
