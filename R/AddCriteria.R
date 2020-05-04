#' Add Criteria Values
#'
#' Adds criteria values to data based on the beneficial use codes
#' @param data Dataframe produced from clean_data()
#' @return Dataframe with added criteria columns
#' @export
#' @examples
#' add_criteria(data = 'result-of-clean_data()')

add_criteria <- function(data) {
  options(scipen = 999)
  parameters <- unique(data$Char_Name)
  
  print("Checking spawn dates...")
  data$spawn_start <- LU_spawn[match(data$SpawnCode, LU_spawn$SpawnCode),"SpawnStart"]
  data$spawn_end <- LU_spawn[match(data$SpawnCode, LU_spawn$SpawnCode),"SpawnEnd"]

  # tmdl_lookup <- odeqstatusandtrends::tmdl_targets

  if(any("Temperature, water" %in% parameters)) {
    print("Adding temperature criteria values...")
    # temp_data <- data %>% dplyr::filter(Char_Name == "Temperature, water")
    # sdadm <- temp_data %>% filter(Statistical_Base == "7DADM")
    
    # data <- dplyr::bind_rows(data[data$Char_Name != "Temperature, water",], sdadm)
    
    data$temp_crit <- Temp_crit[match(data$FishCode, Temp_crit$FishUse_code), "Temp_Criteria"]
    data$criteria <- "Temperature standard"
  }
  if(any("Dissolved oxygen (DO)" %in% parameters)) {
    print("Adding dissolved oxygen criteria values...")
    data$Do_crit_30D <- DO_crit[match(data$DO_code, DO_crit$DO_code), "crit_30D"]
    data$Do_crit_7Mi <- DO_crit[match(data$DO_code, DO_crit$DO_code), "crit_7Mi"]
    data$DO_crit_min <- DO_crit[match(data$DO_code, DO_crit$DO_code), "crit_Min"]
    data$Do_crit_instant <- DO_crit[match(data$DO_code, DO_crit$DO_code), "crit_Instant"]
    data$criteria <- "DO standard"
  }
  if(any("pH" %in% parameters)) {
    print("Adding pH criteria values...")
    data$pH_Min <- pH_crit[match(data$pH_code, pH_crit$pH_code), "pH_Min"]
    data$pH_Max <- pH_crit[match(data$pH_code, pH_crit$pH_code), "pH_Max"]
    data$criteria <- "pH standard"
  }
  if(any(parameters %in% c("Escherichia coli", "Fecal Coliform", "Enterococcus"))) {
    print("adding bacteria criteria values...")
    data$bact_crit_ss <- Bact_crit[match(data$BacteriaCode, Bact_crit$BacteriaCode), "SS_Crit"]
    data$bact_crit_geomean <- Bact_crit[match(data$BacteriaCode, Bact_crit$BacteriaCode), "Geomean_Crit"]
    data$bact_crit_percent <- Bact_crit[match(data$BacteriaCode, Bact_crit$BacteriaCode), "Perc_Crit"]
    data$criteria <- "Bacteria standard"
  }

  # if(any(parameters %in% c("Total suspended solids"))) {
  #   print("Looking for TSS target values...")
  #   tss_targets <- tmdl_lookup %>% dplyr::filter(Parameter == "TSS (mg/L)")
  #   data_tss <- data %>% dplyr::filter(Char_Name == "Total suspended solids")
  #   data_tss <- merge(data_tss, unique(tss_targets[,c("Reach_codes", "summer_target", "summer_start", "summer_end", "winter_target")]),
  #                     by.x = "Reachcode", by.y = "Reach_codes", all.x = TRUE, all.y = FALSE)
  #   if(!"summer_target" %in% colnames(data_tss)){
  #     data_tss <- data_tss %>% dplyr::mutate(summer_target = NaN, summer_start = NA_character_, summer_end = NA_character_, winter_target = NA_character_)
  #   }
  #   data_tss$summer_start <- dplyr::if_else(!is.na(data_tss$summer_start),
  #                                           paste0(data_tss$summer_start, "-", lubridate::year(data_tss$sample_datetime)),
  #                                           NA_character_)
  #   data_tss$summer_start <- as.POSIXct(data_tss$summer_start, format = "%d-%b-%Y")
  #   data_tss$summer_end <- dplyr::if_else(!is.na(data_tss$summer_end),
  #                                         paste0(data_tss$summer_end, "-", lubridate::year(data_tss$sample_datetime)),
  #                                         NA_character_)
  #   data_tss$summer_end <- as.POSIXct(data_tss$summer_end, format = "%d-%b-%Y")
  #   data_tss$TSS_crit <- dplyr::if_else(data_tss$sample_datetime >= data_tss$summer_start & data_tss$sample_datetime < data_tss$summer_end,
  #                                       data_tss$summer_target, data_tss$winter_target)
  #   data_tss <- data_tss %>% dplyr::select(-summer_target, -summer_start, -summer_end, -winter_target)
  #   
  #   data <- dplyr::bind_rows(data[data$Char_Name != "Total suspended solids",], data_tss)
  # }
  # if(any(parameters %in% c(odeqstatusandtrends::AWQMS_Char_Names('TP')))) {
  #   print("Looking for TP target values...")
  #   tp_targets <- tmdl_lookup %>% dplyr::filter(Parameter == "TP (mg/L)")
  #   data_tp <- data %>% dplyr::filter(Char_Name == odeqstatusandtrends::AWQMS_Char_Names('TP'))
  #   
  #   data_tp <- merge(data_tp, unique(tp_targets[,c("Reach_codes", "summer_target", "summer_start", "summer_end", "winter_target", "stat.base")]),
  #                    by.x = "Reachcode", by.y = "Reach_codes", all.x = TRUE, all.y = FALSE)
  #   if(!"summer_target" %in% colnames(data_tp)){
  #     data_tp <- data_tp %>% dplyr::mutate(summer_target = NaN, summer_start = NA_character_, summer_end = NA_character_, winter_target = NA_character_)
  #   }
  #   
  #   data_tp$summer_start <- dplyr::if_else(!is.na(data_tp$summer_start),
  #                                          paste0(data_tp$summer_start, "-", lubridate::year(data_tp$sample_datetime)),
  #                                          NA_character_)
  #   data_tp$summer_start <- as.POSIXct(data_tp$summer_start, format = "%d-%b-%Y")
  #   data_tp$summer_end <- dplyr::if_else(!is.na(data_tp$summer_end),
  #                                        paste0(data_tp$summer_end, "-", lubridate::year(data_tp$sample_datetime)),
  #                                        NA_character_)
  #   data_tp$summer_end <- as.POSIXct(data_tp$summer_end, format = "%d-%b-%Y")
  #   data_tp$tp_summer <- dplyr::if_else(data_tp$sample_datetime >= data_tp$summer_start & data_tp$sample_datetime < data_tp$summer_end,
  #                                       1, 0)
  #   data_tp$TP_crit <- dplyr::if_else(data_tp$sample_datetime >= data_tp$summer_start & data_tp$sample_datetime < data_tp$summer_end,
  #                                     data_tp$summer_target, data_tp$winter_target)
  #   data_tp <- data_tp %>% dplyr::select(-summer_target, -summer_start, -summer_end, -winter_target)
  #   data <- dplyr::bind_rows(data[data$Char_Name != odeqstatusandtrends::AWQMS_Char_Names('TP'),], data_tp)
  # }
  
  return(data)
}
