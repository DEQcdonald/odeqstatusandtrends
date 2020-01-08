#' Calculate excursion statistics for status years
#'
#' Creates a dataframe with excursion statistics by station, parmameter, and status year bin.
#' Excursion statistics include number of excursions, percent excursion, and the max, median, and minimum for results
#' that contributed to an excursion.
#'
#' @param data Dataframe of data to calculate excursion stats from.
#' @param year_range Years from which to determine status. If null, the year range for data provided is used.
#' The range of years in status years will be binned by this number
#' @return Dataframe of stations
#' @export
#' @examples
#' excursion_stats(data = data.frame, year_range = c(start_year, end_year))


excursion_stats <- function(data, year_range = NULL) {

  # This assumes a 20 year period
  status_period <- 4

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

    excursion_stat_df1 <- data %>%
      dplyr::filter(year %in% years,
                    !(BacteriaCode == 3 & Char_Name == "Fecal Coliform")) %>%
      dplyr::filter(excursion_cen==1) %>%
      dplyr::group_by(MLocID, Char_Name, bin) %>%
      dplyr::summarise(excursion_max = max(Result_Numeric, na.rm = TRUE),
                       excursion_median = median(Result_Numeric, na.rm = TRUE),
                       excursion_min = min(Result_Numeric, na.rm = TRUE)) %>%
      dplyr::ungroup()

    excursion_stat_df2 <- data %>%
      dplyr::filter(year %in% years,
                    !(BacteriaCode == 3 & Char_Name == "Fecal Coliform")) %>%
      dplyr::group_by(MLocID, Char_Name, bin) %>%
      dplyr::summarise(results_n = n(),
                       excursions_n = sum(excursion_cen, na.rm = TRUE),
                       percent_excursion = round(excursions_n/results_n*100,0)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(by=c("MLocID", "Char_Name", "bin"), y=excursion_stat_df1) %>%
      tidyr::pivot_wider(names_from=bin, values_from=c(percent_excursion, results_n, excursions_n, excursion_max, excursion_median, excursion_min))

    if(any(data$year %in% years & data$BacteriaCode == 3 & data$Char_Name == "Fecal Coliform")){
      shell_per_excursion <- data %>%
        dplyr::filter(year %in% years,
                      BacteriaCode == 3,
                      Char_Name == "Fecal Coliform") %>%
        dplyr::group_by(MLocID, Char_Name, bin) %>%
        dplyr::summarise(results_n = n(),
                         median = if_else(results_n >= 5, median(Result_cen), NaN),
                         excursions_n = sum(perc_exceed),
                         bact_crit_percent = first(bact_crit_percent),
                         bact_crit_ss = first(bact_crit_ss),
                         n_years = length(unique(year)),
                         excursion = dplyr::if_else((!is.na(median) & median > bact_crit_ss),
                                                    1,
                                                    if_else(results_n >= 10 & excursions_n/results_n > 0.10,
                                                            1,
                                                            if_else(results_n >= 5 & results_n <= 9 & excursions_n >= 1,
                                                                    1, 0)
                                                    )),
                         percent_excursion = ifelse(!is.na(excursion),
                                                if_else(excursion == 1, 100, 0),
                                                NaN)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-excursions_n) %>%
        tidyr::pivot_wider(names_from=bin, values_from=c(percent_excursion, results_n))

      if(nrow(shell_per_excursion) >0){
        excursion_stat_df2 <- dplyr::bind_rows(excursion_stat_df2, shell_per_excursion)
      }

    }

    cols <- c(colnames(excursion_stat_df2[,c(1,2, rev(3:length(colnames(excursion_stat_df2))))]))

    for(i in cols[!cols %in% colnames(excursion_stat_df2)]){
      excursion_stat_df2[,i] <- NaN
    }

    excursion_stat_df2 <- excursion_stat_df2[,cols]

    print(paste("Data should be sufficient for percent exceedance calculations at ", NROW(excursion_stat_df2), "different stations."))

  } else {
    excursion_stat_df2 <- "No stations meet criteria"
    return(excursion_stat_df2)
  }
  return(excursion_stat_df2[,c(1,2,rev(3:length(colnames(excursion_stat_df2))))])
}
