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

  if(any(!unique(df$Char_Name) %in% c("Dissolved oxygen (DO)", "pH"))){
    status_check <- df %>%
      dplyr::filter(!(BacteriaCode == 3 & Char_Name == "Fecal Coliform"),
                    !Char_Name %in% c("Dissolved oxygen (DO)", "pH")) %>%
      dplyr::group_by(MLocID, Char_Name, status_period) %>%
      dplyr::summarise(samples = n(),
                       status = dplyr::if_else(samples < 1 | is.na(samples) | all(is.na(excursion_cen)),
                                               "Unassessed",
                                               dplyr::if_else(any(excursion_cen %in% 1, na.rm = TRUE),
                                                              "Not Attaining",
                                                              "Attaining")
                       ),
                       reason = dplyr::if_else(status == "Unassessed",
                                               dplyr::if_else(any(excursion_cen %in% 2),
                                                              "no_results",
                                                              dplyr::if_else(all(is.na(excursion_cen)),
                                                                      "no_target",
                                                                      "no_results")
                                               ),
                                               NA_character_)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-samples)

    status_reason <<- bind_rows(status_reason, filter(status_check, status == "Unassessed"))

    status_check <- status_check %>%
      select(-reason) %>%
      tidyr::pivot_wider(names_from = status_period, values_from = status)
  } else {status_check <- NULL}

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
                                                                 dplyr::if_else(samples >= 5 & samples <= 9 & excursions > 1,
                                                                                1, 0)
                                                  )),
                       status = dplyr::if_else(samples < 1 | is.na(samples) | all(is.na(excursion)),
                                               "Unassessed",
                                               dplyr::if_else(any(excursion %in% 1, na.rm = TRUE),
                                                              "Not Attaining",
                                                              "Attaining")
                       ),
                       reason = dplyr::if_else(status == "Unassessed",
                                               dplyr::if_else(samples < 5,
                                                              "less_than_5_samples",
                                                              if_else(all(is.na(excursion)),
                                                                      "no_target",
                                                                      "no_results")
                                                              ),
                                               NA_character_)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(MLocID, Char_Name, status_period, status, reason)

    if(nrow(status_reason) > 0){
      status_reason <<- dplyr::bind_rows(status_reason, shell_status)
    }

    shell_status <- shell_status %>%
      select(-reason) %>%
      tidyr::pivot_wider(names_from = status_period, values_from = status)

    if(nrow(shell_status) >0){
      status_check <- dplyr::bind_rows(status_check, shell_status)
    }

  }

  if(any(unique(df$Char_Name) == "pH")){
    status_check_pH <- df %>%
      dplyr::filter(Char_Name == "pH") %>%
      dplyr::group_by(MLocID, Char_Name, status_period) %>%
      dplyr::summarise(samples = n(),
                       n_excursion = sum(excursion_cen, na.rm = TRUE),
                       binomial_excursions = odeqassessment::excursions_req(samples),
                       per_exceed = dplyr::if_else(samples >= 5 & n_excursion > binomial_excursions,
                                                   1, 0),
                       status = dplyr::if_else(samples < 5 | is.na(samples) | all(is.na(excursion_cen)),
                                               "Unassessed",
                                               dplyr::if_else(per_exceed == 1,
                                                              "Not Attaining",
                                                              "Attaining")
                       ),
                       reason = dplyr::if_else(status == "Unassessed",
                                               dplyr::if_else(samples < 5,
                                                              "no_results",
                                                              if_else(all(is.na(excursion_cen)),
                                                                      "no_target",
                                                                      "no_results")
                                               ),
                                               NA_character_)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-samples, -n_excursion, -binomial_excursions, -per_exceed)

    status_reason <<- bind_rows(status_reason, filter(status_check_pH, status == "Unassessed"))

    status_check_pH <- status_check_pH %>%
      select(-reason) %>%
      tidyr::pivot_wider(names_from = status_period, values_from = status)

    if(nrow(status_check_pH) > 0){
      status_check <- dplyr::bind_rows(status_check, status_check_pH)
    }
  }

  if(any(unique(df$Char_Name) == "Dissolved oxygen (DO)")){
    status_check_DO <- df %>%
      dplyr::filter(Char_Name == "Dissolved oxygen (DO)") %>%
      dplyr::group_by(MLocID, Char_Name, status_period) %>%
      dplyr::summarise(samples = n(),
                       spawn_samples = sum(Spawn_type == "Spawn", na.rm = TRUE),
                       n_yr_inst_exc = max(sum(yr_exc_inst, na.rm = TRUE), sum(yr_exc_min, na.rm = TRUE)),
                       n_spawn_inst_exc = max(sum(spwn_exc_inst == 1 & Spawn_type == "Spawn", na.rm = TRUE),
                                              sum(spwn_exc_min == 1 & Spawn_type == "Spawn", na.rm = TRUE)),
                       yr_binomial_excursions = odeqassessment::excursions_req(samples),
                       spawn_binomial_excursions = odeqassessment::excursions_req(spawn_samples),
                       yr_per_exceed = dplyr::if_else(samples >= 5 & n_yr_inst_exc > yr_binomial_excursions,
                                                   1, 0),
                       spawn_per_exceed = dplyr::if_else(spawn_samples >= 5 & n_spawn_inst_exc > spawn_binomial_excursions,
                                                         1, 0),
                       n_critical_30D = sum(is.crit == 1 & Statistical_Base == "30DADMean", na.rm = TRUE),
                       n_critical_7D = sum(Spawn_type == "Spawn" & Statistical_Base == "7DADMean", na.rm = TRUE),
                       yr_crit = n_critical_30D >= 15,
                       spawn_crit = n_critical_7D >= 15,
                       yr_status = dplyr::if_else(yr_crit,
                                                  dplyr::if_else(any(1 %in% c(yr_exc_30DADMean, yr_exc_7DADMin, yr_exc_min)),
                                                                 "Not Attaining",
                                                                 "Attaining"),
                                                  dplyr::if_else(samples >= 5,
                                                                 dplyr::if_else(n_yr_inst_exc > yr_binomial_excursions,
                                                                                "Not Attaining",
                                                                                "Attaining"),
                                                                 "Unassessed"
                                                  )
                       ),
                       spawn_status = dplyr::if_else(spawn_crit,
                                                     dplyr::if_else(any(1 %in% c(spwn_exc_7DADMean, spwn_exc_min)),
                                                                    "Not Attaining",
                                                                    "Attaining"),
                                                     dplyr::if_else(samples >= 5,
                                                                    dplyr::if_else(n_spawn_inst_exc > spawn_binomial_excursions,
                                                                                   "Not Attaining",
                                                                                   "Attaining"),
                                                                    "Unassessed"
                                                     )
                       ),
                       status = dplyr::if_else("Not Attaining" %in% c(yr_status, spawn_status),
                                               "Not Attaining",
                                               dplyr::if_else("Attaining" %in% c(yr_status, spawn_status),
                                                              "Attaining",
                                                              "Unassessed")
                       ),
                       reason = dplyr::if_else(status == "Unassessed",
                                               "no_results",
                                               NA_character_)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(MLocID, Char_Name, status_period, status, reason)

    status_reason <<- bind_rows(status_reason, filter(status_check_DO, status == "Unassessed"))

    status_check_DO <- status_check_DO %>%
      select(-reason) %>%
      tidyr::pivot_wider(names_from = status_period, values_from = status)

    if(nrow(status_check_DO) > 0){
      status_check <- dplyr::bind_rows(status_check, status_check_DO)
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
