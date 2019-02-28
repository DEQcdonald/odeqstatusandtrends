#' Determine stations with sufficient data for trend analysis
#'
#'
#' @param data Dataframe to determine trend years from.
#' @param trend.years Which years to determine status by. Default is current year minus 2 to present year.
#' @return dataframe of stations with sufficient data
#' @export
#' @example
#' trend_stns(data = data.frame, status_years = c("current-year", "2-years-ago"))

trend_stns <- function(data, trend_years = c(format(min(data$sample_datetime), "%Y"):format(Sys.Date(), "%Y"))) {

  if(length(trend_years) < 8){break("Number of years should be less than or equal to 8")}

  data$year <- lubridate::year(data$sample_datetime)

  if(any(unique(data$year) %in% trend_years)){
    trend_check <- data %>%
      filter(year %in% trend_years) %>%
      dplyr::group_by(MLocID, Char_Name) %>%
      dplyr::summarise(n_years = length(unique(year)),
                       avg_obs = n()/n_years,
                       min_year = min(year),
                       max_year = max(year)) %>%
      filter(n_years>=8)

    print(paste("Data may be sufficient for", NROW(trend_check), "different trends to be determined."))

  } else {
    trend_check <- "No stations meet trend year criteria"
    print(trend_check)
  }
  return(trend_check)
}
