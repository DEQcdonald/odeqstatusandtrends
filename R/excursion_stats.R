#' Calculate excursion statistics for status periods
#'
#' Creates a dataframe with excursion statistics by station, parmameter, and status period.
#' Excursion statistics include number of excursions, percent excursion, and the max, median, and minimum for results
#' that contributed to an excursion.
#'
#' @param data_assessed Dataframe returned from an assessment fucntion in odeqassessment package.
#'  data_assessed must include a grouping column called 'status_period' which can be created by running odeqstatusandtrends::status_period().
#' @return Dataframe of station excursion information
#' @export
#' @examples
#' excursion_stats(data_assessed=data_assessed)

excursion_stats <- function(data_assessed) {

  if(is.null(data_assessed)) {
    warning("There are no results in data_assessed.")
    excursion_stat_df2 <- "No stations meet criteria"
    return(excursion_stat_df2)
  }

  if(!"status_period" %in% colnames(data_assessed)) {
    stop("There is no 'status_period' column defined in data_assessed. Run odeqstatusandtrends::status_period().")
  }

  if(any(is.na(data_assessed$status_period))) {
    warning("NA's present in 'status_period' column.")
  }

  if(!"MLocID" %in% colnames(data_assessed)) {
    stop("There is no 'MLocID' column defined in data_assessed.")
  }

  if(!"Char_Name" %in% colnames(data_assessed)) {
    stop("There is no 'Char_Name' column defined in data_assessed.")
  }

  if(!"Result_Numeric" %in% colnames(data_assessed)) {
    stop("There is no 'Result_Numeric' column defined in data_assessed.")
  }

  if(!"excursion_cen" %in% colnames(data_assessed)) {
    stop("There is no 'excursion_cen' column defined in data_assessed.")
  }

  if(!"BacteriaCode" %in% colnames(data_assessed)) {
    stop("There is no 'BacteriaCode' column defined in data_assessed.")
  }


  excursion_stat_df1 <- data_assessed %>%
    dplyr::filter(!(BacteriaCode == 3 & Char_Name == "Fecal Coliform")) %>%
    dplyr::filter(excursion_cen==1) %>%
    dplyr::group_by(MLocID, Char_Name, status_period) %>%
    dplyr::summarise(excursion_max = max(Result_Numeric, na.rm = TRUE),
                     excursion_median = median(Result_Numeric, na.rm = TRUE),
                     excursion_min = min(Result_Numeric, na.rm = TRUE)) %>%
    dplyr::ungroup()

  excursion_stat_df2 <- data_assessed %>%
    dplyr::filter(!(BacteriaCode == 3 & Char_Name == "Fecal Coliform")) %>%
    dplyr::group_by(MLocID, Char_Name, status_period) %>%
    dplyr::summarise(results_n = n(),
                     excursions_n = sum(excursion_cen, na.rm = TRUE),
                     percent_excursion = round(excursions_n/results_n*100,0)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(by=c("MLocID", "Char_Name", "status_period"), y=excursion_stat_df1) %>%
    tidyr::pivot_wider(names_from=status_period, values_from=c(percent_excursion, results_n, excursions_n, excursion_max, excursion_median, excursion_min))

  if(any(data_assessed$BacteriaCode == 3 & data_assessed$Char_Name == "Fecal Coliform")){

    if(!"Result_cen" %in% colnames(data_assessed)) {
      stop("There is no 'Result_cen' column defined in data_assessed.")
    }

    if(!"perc_exceed" %in% colnames(data_assessed)) {
      stop("There is no 'perc_exceed' column defined in data_assessed.")
    }

    if(!"bact_crit_percent" %in% colnames(data_assessed)) {
      stop("There is no 'bact_crit_percent' column defined in data_assessed.")
    }

    if(!"bact_crit_ss" %in% colnames(data_assessed)) {
      stop("There is no 'bact_crit_ss' column defined in data_assessed.")
    }

    shell_per_excursion <- data_assessed %>%
      dplyr::filter(BacteriaCode == 3,
                    Char_Name == "Fecal Coliform") %>%
      dplyr::group_by(MLocID, Char_Name, status_period) %>%
      dplyr::summarise(results_n = n(),
                       median = if_else(results_n >= 5, median(Result_cen, na.rm = TRUE), NaN),
                       excursions_n = sum(perc_exceed),
                       bact_crit_percent = first(bact_crit_percent),
                       bact_crit_ss = first(bact_crit_ss),
                       n_years = length(unique(year)),
                       excursion = dplyr::if_else((!is.na(median) & median > bact_crit_ss),
                                                  1,
                                                  dplyr::if_else(results_n >= 10 & excursions_n/results_n > 0.10,
                                                          1,
                                                          dplyr::if_else(results_n >= 5 & results_n <= 9 & excursions_n >= 1,
                                                                  1, 0)
                                                  )),
                       percent_excursion = ifelse(!is.na(excursion),
                                                  dplyr::if_else(excursion == 1, 100, 0),
                                                  NaN)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-excursions_n) %>%
      tidyr::pivot_wider(names_from=status_period, values_from=c(percent_excursion, results_n))

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

  return(excursion_stat_df2[,c(1,2,rev(3:length(colnames(excursion_stat_df2))))])
}
