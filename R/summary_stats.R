#' Calculate summary statistics for status periods
#'
#' Creates a dataframe with statistics by station, parmameter, and status period.
#' Statistics include number of results, the max, median, and minimum for result value.
#'
#' @param df Dataframe returned from an assessment fucntion in odeqassessment package.
#'  df must include a grouping column called 'status_period' which can be created by running odeqstatusandtrends::status_period().
#' @return Dataframe of station excursion information
#' @export
#' @examples
#' summary_stats(df=data_assessed)

summary_stats <- function(df) {

  if(is.null(df)) {
    warning("There are no results in df.")
    stat_df2 <- "No stations meet criteria"
    return(stat_df2)
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

  if(!"Spawn_type" %in% colnames(df)) {
    stop("There is no Spawn_type' column defined in df.")
  }

  if(!"Result_Numeric" %in% colnames(df)) {
    stop("There is no 'Result_Numeric' column defined in df.")
  }

  if(!"BacteriaCode" %in% colnames(df)) {
    stop("There is no 'BacteriaCode' column defined in df.")
  }

  stat_df1 <- df %>%
    dplyr::filter(!(BacteriaCode == 3 & Char_Name == "Fecal Coliform")) %>%
    dplyr::group_by(MLocID, Char_Name, Spawn_type, status_period) %>%
    dplyr::summarise(max = max(Result_Numeric, na.rm = TRUE),
                     median = median(Result_Numeric, na.rm = TRUE),
                     min = min(Result_Numeric, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(min=dplyr::if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(min, 1), min),
                  median=dplyr::if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(median, 1), median),
                  max=dplyr::if_else(grepl("Temperature|Dissolved oxygen", Char_Name), round(max, 1), max)) %>%
    tidyr::pivot_wider(names_from=status_period, values_from=c(max, median, min))

  if(any(df$BacteriaCode == 3 & df$Char_Name == "Fecal Coliform")){

    if(!"Result_cen" %in% colnames(df)) {
      stop("There is no 'Result_cen' column defined in df.")
    }

    shell_stat <- df %>%
      dplyr::filter(BacteriaCode == 3,
                    Char_Name == "Fecal Coliform") %>%
      dplyr::mutate(status_period = paste0("median_", status_period)) %>%
      dplyr::group_by(MLocID, Char_Name, Spawn_type, status_period) %>%
      dplyr::summarise(results_n = n(),
                       median = dplyr::if_else(results_n >= 5, median(Result_cen, na.rm = TRUE), NaN)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-results_n) %>%
      tidyr::pivot_wider(names_from=status_period, values_from=c(median))

    if(nrow(shell_stat) >0){
      stat_df2 <- dplyr::bind_rows(stat_df1, shell_stat)
    }

  } else {
    stat_df2 <- stat_df1
  }

  cols <- c(colnames(stat_df2[,c(1,2, rev(3:length(colnames(stat_df2))))]))

  for(i in cols[!cols %in% colnames(stat_df2)]){
    stat_df2[,i] <- NaN
  }

  stat_df2 <- stat_df2[,cols]

  print(paste("Data should be sufficient summary stat calculations at ", NROW(stat_df2), "different stations."))

  return(stat_df2[,c(1,2,rev(3:length(colnames(stat_df2))))])
}
