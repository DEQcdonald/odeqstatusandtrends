#' Determine percent excursion for status years
#'
#' Creates a dataframe with stations, the number of years with data within
#' the status years, and the percent excursion of the results by parameter.
#' @param data Dataframe to determine status from.
#' @param year_range Years from which to determine status. If null, the year range for data provided is used.
#' @param status_period Number of whole years to include in a status analysis.
#' The range of years in status years will be binned by this number
#' @return Dataframe of stations with sufficient data
#' @export
#' @examples
#' percent_excursion(data = data.frame, year_range = c(start_year, end_year), status_period = 5)

percent_excursion <- function(data, year_range = NULL, status_period = 4) {

  if("Phosphate-phosphorus" %in% unique(data$Char_Name)){
    if("tp_year" %in% colnames(data)){
      data$year <- dplyr::if_else(is.na(data$tp_year), lubridate::year(data$sample_datetime), data$tp_year)
    } else {data$year <- lubridate::year(data$sample_datetime)}
  } else {data$year <- lubridate::year(data$sample_datetime)}

  if(is.null(year_range)){
    year_range <- c(min(data$year, na.rm = TRUE), max(data$year, na.rm = TRUE))
  }

  years <- year_range[2]:year_range[1]
  breaks <- seq(year_range[2], year_range[1], by = -4)
  cols <- sapply(breaks, function(x){
    start <- x - status_period + 1
    return(paste0(
      "percent_excursion_",
      start, "_", x))
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

    per_excursion_df <- data %>%
      dplyr::filter(year %in% years,
                    !(BacteriaCode == 3 & Char_Name == "Fecal Coliform")) %>%
      dplyr::group_by(MLocID, Char_Name, bin) %>%
      dplyr::summarise(results_n = n(),
                       excursions = sum(excursion_cen, na.rm = TRUE),
                       per_excursion = round(excursions/results_n*100),0) %>%
      dplyr::ungroup() %>%
      dplyr::select(-excursions) %>%
      tidyr::pivot_wider(names_from=bin, values_from=c(per_excursion, results_n))

    if(any(data$year %in% years & data$BacteriaCode == 3 & data$Char_Name == "Fecal Coliform")){
      shell_per_excursion <- data %>%
        dplyr::filter(year %in% years,
                      BacteriaCode == 3,
                      Char_Name == "Fecal Coliform") %>%
        dplyr::group_by(MLocID, Char_Name, bin) %>%
        dplyr::summarise(results_n = n(),
                         median = if_else(results_n >= 5, median(Result_cen), NaN),
                         excursions = sum(perc_exceed),
                         bact_crit_percent = first(bact_crit_percent),
                         bact_crit_ss = first(bact_crit_ss),
                         n_years = length(unique(year)),
                         excursion = dplyr::if_else((!is.na(median) & median > bact_crit_ss),
                                                    1,
                                                    if_else(results_n >= 10 & excursions/results_n > 0.10,
                                                            1,
                                                            if_else(results_n >= 5 & results_n <= 9 & excursions >= 1,
                                                                    1, 0)
                                                    )),
                         per_excursion = ifelse(!is.na(excursion),
                                                if_else(excursion == 1, 100, 0),
                                                NaN)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-excursions) %>%
        tidyr::pivot_wider(names_from=bin, values_from=c(per_excursion, results_n))

      if(nrow(shell_per_excursion) >0){
        per_excursion_df <- dplyr::bind_rows(per_excursion_df, shell_per_excursion)
      }

    }

    cols <- c(colnames(per_excursion_df[,c(1,2, rev(3:length(colnames(per_excursion_df))))]))

    for(i in cols[!cols %in% colnames(per_excursion_df)]){
      per_excursion_df[,i] <- NaN
    }

    per_excursion_df <- per_excursion_df[,cols]

    print(paste("Data should be sufficient for percent exceedance calculations at ", NROW(per_excursion_df), "different stations."))

  } else {
    per_excursion_df <- "No stations meet criteria"
    return(per_excursion_df)
  }
  return(per_excursion_df[,c(1,2,rev(3:length(colnames(per_excursion_df))))])
}
