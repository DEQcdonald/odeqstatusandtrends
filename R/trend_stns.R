#' Determine stations with sufficient data for trend analysis
#'
#' Creates a dataframe with stations and the number of years with data. Stations with less than 8 different years of data
#' are not sufficient for trend analysis via the status and trends methods.
#' @param data Dataframe to determine stations for trend analysis
#' @param trend.years Which years to determine trend by. Default is the minimum year within the data to the current year.
#' @return Dataframe of stations with sufficient years of data
#' @export
#' @examples
#' trend_stns(df = data_assessed, trend_years = c(format(min(data$sample_datetime), "%Y"):format(Sys.Date(), "%Y")))

trend_stns <- function(df, trend_years=NULL) {

  if(!"sample_datetime" %in% colnames(df)) {
    stop("There is no 'sample_datetime' column defined in df.")
  }

  if(!"MLocID" %in% colnames(df)) {
    stop("There is no 'MLocID' column defined in df.")
  }

  if(!"Char_Name" %in% colnames(df)) {
    stop("There is no 'Char_Name' column defined in df.")
  }

  if(!"Statistical_Base" %in% colnames(df)) {
    stop("There is no 'Statistical_Base' column defined in df.")
  }

  if(is.null(trend_years)){
  trend_years <- c(format(min(df$sample_datetime, na.rm = TRUE), "%Y"):format(Sys.Date(), "%Y"))
  }

  if(length(trend_years) < 8){
    print("Number of years should be more than or equal to 8")
    trend_check <- NULL
  } else {

    df$year <- lubridate::year(df$sample_datetime)

    if(any(unique(df$year) %in% trend_years)){
      trend_check <- df %>%
        dplyr::filter(year %in% trend_years) %>%
        dplyr::group_by(MLocID, Char_Name, Statistical_Base) %>%
        dplyr::summarise(n_years = length(unique(year)),
                         avg_obs = dplyr::n()/n_years,
                         min_year = min(year),
                         max_year = max(year)) %>%
        dplyr::filter(n_years>=8)

      print(paste("Data may be sufficient for", NROW(trend_check), "different trends to be determined."))

    } else {
      trend_check <- "No stations meet trend year criteria"
      print(trend_check)
    }
  }
  return(trend_check)
}
