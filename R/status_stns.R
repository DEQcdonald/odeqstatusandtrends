#' Determine stations with sufficient data for status analysis
#'
#' Creates a dataframe with stations, the number of years with data within
#' the status years, and whether or not there were any exceedances.
#' @param df Dataframe returned from an assessment function in odeqassessment package.
#'  df must include a grouping column called 'status_period' which can be created by running odeqstatusandtrends::status_period().
#' @return Dataframe of stations with sufficient data
#' @export
#' @examples
#' status_stns(df = data_assessed)

status_stns <- function(df) {

  if(is.null(df)) {
    warning("There are no results in df.")
    status_check <- "No stations meet criteria"
    return(status_check)
  }

  if(!"status_period" %in% colnames(df)) {
    stop("There is no 'status_period' column defined in df. Run odeqstatusandtrends::status_period().")
  }

  if(any(is.na(df$status_period))) {
    warning("NA's present in 'status_period' column.")
  }

  if(!"MLocID" %in% colnames(df)) {
    stop("There is no 'MLocID' column defined in df.")
  }

  if(!"Char_Name" %in% colnames(df)) {
    stop("There is no 'Char_Name' column defined in df.")
  }

  if(!"Result_Numeric" %in% colnames(df)) {
    stop("There is no 'Result_Numeric' column defined in df.")
  }

  if(!"excursion_cen" %in% colnames(df)) {
    stop("There is no 'excursion_cen' column defined in df.")
  }

  if(!"BacteriaCode" %in% colnames(df)) {
    stop("There is no 'BacteriaCode' column defined in df.")
  }

  status_check <- df %>%
    dplyr::filter(!(BacteriaCode == 3 & Char_Name == "Fecal Coliform")) %>%
    dplyr::group_by(MLocID, Char_Name, status_period) %>%
    dplyr::summarise(samples = n(),
                     status = dplyr::if_else(samples < 1 | is.na(samples) | all(is.na(excursion_cen)),
                                             "Unassessed",
                                             dplyr::if_else(any(excursion_cen == 1, na.rm = TRUE),
                                                            "Not Attaining",
                                                            "Attaining")
                     )
    ) %>%
    dplyr::ungroup() %>% select(-samples) %>%
    tidyr::spread(key = status_period, value = status)

  if(any(df$BacteriaCode == 3 & df$Char_Name == "Fecal Coliform")){
    shell_status <- df %>%
      dplyr::filter(BacteriaCode == 3,
                    Char_Name == "Fecal Coliform") %>%
      dplyr::group_by(MLocID, Char_Name, status_period) %>%
      dplyr::summarise(samples = n(),
                       median = dplyr::if_else(samples >= 5, median(Result_cen, na.rm = TRUE), NaN),
                       excursions = sum(perc_exceed),
                       bact_crit_percent = dplyr::first(bact_crit_percent), # 43 organisms per 100mL, requires 10% exceedance
                       bact_crit_ss = dplyr::first(bact_crit_ss), # 14 organisms per 100mL, median used to evaluate excursion
                       # n_years = length(unique(year)),
                       excursion = dplyr::if_else((!is.na(median) & median > bact_crit_ss),
                                                  1,
                                                  dplyr::if_else(samples >= 10 & excursions/samples > 0.10,
                                                                 1,
                                                                 dplyr::if_else(samples >= 5 & samples <= 9 & excursions >= 1,
                                                                                1, 0)
                                                  )),
                       status = dplyr::if_else(samples < 1 | is.na(samples) | all(is.na(excursion)),
                                               "Unassessed",
                                               dplyr::if_else(any(excursion == 1, na.rm = TRUE),
                                                              "Not Attaining",
                                                              "Attaining")
                       )
      ) %>%
      dplyr::ungroup() %>% dplyr::select(MLocID, Char_Name, status_period, status) %>% tidyr::pivot_wider(names_from=status_period, values_from=status)

    if(nrow(shell_status) >0){
      status_check <- dplyr::bind_rows(status_check, shell_status)
    }

  }

  status_check[is.na(status_check)] <- "Unassessed"
  cols <- c("MLocID", "Char_Name", cols)

  for(i in cols[!cols %in% colnames(status_check)]){
    status_check[,i] <- "Unassessed"
  }

  status_check <- status_check[,cols]

  print(paste("Data should be sufficient for", NROW(status_check), "different statuses to be determined."))

  return(status_check[,c(1,2,rev(3:length(colnames(status_check))))])
}
