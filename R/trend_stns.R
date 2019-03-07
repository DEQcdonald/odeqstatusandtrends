#' Determine stations with sufficient data for trend analysis
#'
#' Creates a dataframe with stations and the number of years with data. Stations with less than 8 different years of data
#' are not sufficient for trend analysis via the status and trends methods.
#' @param data Dataframe to determine stations for trend analysis
#' @param trend.years Which years to determine trend by. Default is the minimum year within the data to the current year.
#' @return Dataframe of stations with sufficient years of data
#' @export
#' @example
#' trend_stns(data = data.frame, status_years = c("current-year", "2-years-ago"))

trend_stns <- function(data, trend_years = c(format(min(data$sample_datetime), "%Y"):format(Sys.Date(), "%Y"))) {

  if(length(trend_years) < 8){stop("Number of years should be less than or equal to 8")}

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
