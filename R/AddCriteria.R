#' Add Criteria Values
#'
#' Adds criteria values to data based on the beneficial use codes
#' @param data Dataframe produced from clean_data()
#' @return Dataframe with added criteria columns
#' @export
#' @example
#' add_criteria(data = 'result-of-clean_data()')

add_criteria <- function(data) {
  parameters <- unique(data$Char_Name)
  if(any("Temperature, water" %in% parameters)) {
    temp_data <- data %>% filter(Char_Name == "Temperature, water")
    sdadm <- temp_data %>% filter(Statistical_Base == "7DADM")

    data <- bind_rows(data[data$Char_Name != "Temperature, water",], sdadm)
    data$temp_crit <- Temp_crit[match(data$FishCode, Temp_crit$FishUse_code), "Temp_Criteria"]
  }
  if(any("Dissolved oxygen (DO)" %in% parameters)) {
    data$Do_crit_30D <- DO_crit[match(data$DO_code, DO_crit$DO_code), "crit_30D"]
    data$Do_crit_7Mi <- DO_crit[match(data$DO_code, DO_crit$DO_code), "crit_7Mi"]
    data$DO_crit_min <- DO_crit[match(data$DO_code, DO_crit$DO_code), "crit_Min"]
    data$Do_crit_instant <- DO_crit[match(data$DO_code, DO_crit$DO_code), "crit_Instant"]
  }
  if(any("pH" %in% parameters)) {
    data$pH_crit_min <- pH_crit[match(data$pH_code, pH_crit$pH_code), "pH_Min"]
    data$pH_crit_max <- pH_crit[match(data$pH_code, pH_crit$pH_code), "pH_Max"]
  }
  if(any(parameters %in% c("Escherichia coli", "Fecal Coliform", "Enterococcus"))) {
    data$bact_crit_ss <- Bact_crit[match(data$BacteriaCode, Bact_crit$BacteriaCode), "SS_Crit"]
    data$bact_crit_geomean <- Bact_crit[match(data$BacteriaCode, Bact_crit$BacteriaCode), "Geomean_Crit"]
    data$bact_crit_percent <- Bact_crit[match(data$BacteriaCode, Bact_crit$BacteriaCode), "Perc_Crit"]
  }

  data$spawn_start <- as.numeric(lubridate::month(as.Date(LU_spawn[match(data$SpawnCode, LU_spawn$SpawnCode),"SpawnStart"], format="%m/%d")))*100 +
    as.numeric(lubridate::day(as.Date(LU_spawn[match(data$SpawnCode, LU_spawn$SpawnCode),"SpawnStart"], format="%m/%d")))
  data$spawn_end <- as.numeric(lubridate::month(as.Date(LU_spawn[match(data$SpawnCode, LU_spawn$SpawnCode),"SpawnEnd"], format="%m/%d")))*100 +
    as.numeric(lubridate::day(as.Date(LU_spawn[match(data$SpawnCode, LU_spawn$SpawnCode),"SpawnEnd"], format="%m/%d")))
  data$sample_mon_year <- as.numeric(lubridate::month(data$sample_datetime))*100 + as.numeric(lubridate::day(data$sample_datetime))
  data$spawning <- if(is.na(data$spawn_start)){
    FALSE
  } else if(data$spawn_start < data$spawn_end & (data$sample_mon_year > data$spawn_start & data$sample_mon_year < data$spawn_end)){
    TRUE
  } else if(data$spawn_start > data$spawn_end & (data$sample_mon_year > data$spawn_start | data$sample_mon_year < data$spawn_end)){
    TRUE
  } else {FALSE}

  return(data)
}
