#' Determine stations with sufiicient data for status analysis
#'
#'
#' @param data Dataframe to determine status from.
#' @param status.years Which years to determine status by. Default is current year minus 2 to present year.
#' @return dataframe of stations with sufficient data
#' @export
#' @example
#' status_stns(data = data.frame, status_years = c("current-year", "2-years-ago"))

status_stns <- function(data, status_years = c((as.numeric(format(Sys.Date(), "%Y"))-2):format(Sys.Date(), "%Y"))) {

  data$year <- lubridate::year(data$sample_datetime)

  if(any(unique(data$year) %in% status_years)){
    status_check <- data %>%
      filter(year %in% status_years) %>%
      dplyr::group_by(MLocID, Char_Name) %>%
      dplyr::summarise(n_years = length(unique(year))) %>%
      filter(n_years>=2)

    print(paste("Data should be sufficient for", NROW(status_check), "different statuses to be determined."))

  } else {
    status_check <- "No stations meet Status criteria"
    print(status_check)
  }
  return(status_check)
}
