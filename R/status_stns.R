#' Determine stations with sufficient data for status analysis
#'
#' Creates a dataframe with stations, the number of years with data within
#' the status years, and whether or not there were any exceedances.
#' @param data Dataframe to determine status from.
#' @param year_range Years from which to determine status. Default is most
#' recent full year and the 20 years previous.
#' @param status_period Number of whole to include in a status analysis.
#' The range of years in status years will be binned by this number
#' @return Dataframe of stations with sufficient data
#' @export
#' @examples
#' status_stns(data = data.frame, year_range = c(start_year, end_year), status_period = 5)

status_stns <- function(data, year_range = c(year(Sys.Date())-21, year(Sys.Date())-1),
                        status_period = 4) {

  data$year <- lubridate::year(data$sample_datetime)
  years <- year_range[2]:year_range[1]
  breaks <- seq(year_range[2], year_range[1], by = -4)
  cols <- sapply(breaks, function(x){
    start <- x - status_period + 1
    return(paste0("_", start, "_", x))
  })
  bins <- lapply(breaks, function(x){
    start <- x - status_period + 1
    return(c(start:x))
  })
  names(bins) <- cols
  data$bin <- sapply(data$year, function(x){
      i <- sapply(bins, function(y){
        return(x %in% y)
      })
      return(names(bins[i]))
    })

  if(any(unique(data$year) %in% year_range)){
    status_check <- data %>%
      filter(year %in% status_years) %>%
      dplyr::group_by(MLocID, Char_Name, bin) %>%
      dplyr::summarise(n_years = length(unique(year)),
                       excursions = sum(excursion_cen, na.rm = TRUE),
                       status = if_else(n_years < 2,
                                        NA_character_,
                                        if_else(any(excursion_cen == 1, na.rm = TRUE), "Attaining", "Not Attaining"))
                       ) %>%
      ungroup() %>% select(-n_years, -excursions) %>%
      spread(key = bin, value = status)

    print(paste("Data should be sufficient for", NROW(status_check), "different statuses to be determined."))

  } else {
    status_check <- "No stations meet Status criteria"
    print(status_check)
  }
  return(status_check)
}
