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
    return(paste0("status_", start, "_", x))
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


   if(any(unique(data$year) %in% years)){

     status_check <- data %>%
       filter(year %in% years,
              !(BacteriaCode == 3 & Char_Name == "Fecal Coliform")) %>%
       dplyr::group_by(MLocID, Char_Name, bin) %>%
       dplyr::summarise(n_years = length(unique(year)),
                        excursions = sum(excursion_cen, na.rm = TRUE),
                        status = if_else(n_years < 1 | is.na(n_years) | all(is.na(excursion_cen)),
                                         "Unassessed",
                                         if_else(n_years < 2,
                                                 "Insufficient Data",
                                                 if_else(any(excursion_cen == 1, na.rm = TRUE),
                                                         "Not Attaining",
                                                         "Attaining")
                                         )
                        )
       ) %>%
       ungroup() %>% select(-n_years, -excursions) %>% spread(key = bin, value = status)

     if(any(data$year %in% years & data$BacteriaCode == 3 & data$Char_Name == "Fecal Coliform")){
       shell_status <- data %>%
         dplyr::filter(year %in% years,
                       BacteriaCode == 3,
                       Char_Name == "Fecal Coliform") %>%
         dplyr::group_by(MLocID, Char_Name, bin) %>%
         dplyr::summarise(num_samples = n(),
                          median = if_else(num_samples >= 5, median(Result_cen), NaN),
                          num_exceed = sum(perc_exceed),
                          bact_crit_percent = first(bact_crit_percent),
                          bact_crit_ss = first(bact_crit_ss),
                          n_years = length(unique(year)),
                          excursion = if_else((!is.na(median) & median > bact_crit_ss),
                                              1,
                                              if_else(num_samples >= 10 & num_exceed/num_samples > 0.10,
                                                      1,
                                                      if_else(num_samples >= 5 & num_samples <= 9 & num_exceed >= 1,
                                                              1, 0)
                                              )),
                          status = if_else(n_years < 1 | is.na(n_years) | all(is.na(excursion)),
                                           "Unassessed",
                                           if_else(n_years < 2,
                                                   "Insufficient Data",
                                                   if_else(excursion == 1,
                                                           "Not Attaining",
                                                           "Attaining")
                                           )
                          )

         ) %>%
         ungroup() %>% select(MLocID, Char_Name, bin, status) %>% spread(key = bin, value = status)

       if(nrow(shell_status) >0){
         status_check <- bind_rows(status_check, shell_status)
       }

     }


    status_check[is.na(status_check)] <- "Unassessed"

    print(paste("Data should be sufficient for", NROW(status_check), "different statuses to be determined."))

  } else {
    status_check <- "No stations meet Status criteria"
    return(status_check)
  }
  return(status_check[,c(1,2,rev(3:length(colnames(status_check))))])
}
