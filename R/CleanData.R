#' Clean Data for Status and Trends Analysis
#'
#' Removes unnecessary variables, checks data for quality, and adds "sample_datetime" and "sample_id" columns.
#' @param data The result of GetData(), expects a STORET formatted dataframe.
#' @return A slimmer dataframe checked for quality controls.
#' @export
#' @examples
#' CleanData(data = result-from-GetData())

CleanData <- function(data)
{
  if(is.null(data)) {
    warning("There are no results in data.")
    return(data)
  }
  
  need_cols <- c('MLocID', 'StationDes', 'OrganizationID', 'Project1', 'Org_Name', 'Lat_DD', 'Long_DD', 'ELEV_Ft', 'Datum', 'HUC8', 'AU_ID',
                 'Reachcode', 'Char_Name','SampleStartDate', 'SampleStartTime', 'Result','Result_Numeric', 'Result_Operator', 'Result_Unit',
                 'Statistical_Base', 'act_depth_height', 'QualifierAbbr', 'Method_Code', 'Activity_Type', 'act_id', 'MRLValue', 'Result_status',
                 'FishCode','SpawnCode', 'WaterTypeCode', 'WaterBodyCode', 'BacteriaCode', 'DO_code', 'ben_use_code', 'pH_code', 'DO_SpawnCode')
  
  if(any(!need_cols %in% names(data))) {
    missing_cols <- paste(need_cols[!(need_cols %in% names(data))], collapse = "', '")
    stop(paste0("The following columns are needed: '", missing_cols,"."))
  }
  
  # Add datetime columns and remove unused variables
  data$sample_datetime <- paste(data$SampleStartDate, data$SampleStartTime)
  data$sample_datetime <- as.POSIXct(data$sample_datetime, format = '%Y-%m-%d %H:%M:%S')
  
  # remove 'SampleStartDate' and 'SampleStartTime' and add 'sample_datetime'
  need_cols <- c(need_cols[c(1:13,16:36)], c('sample_datetime'))
  
  print("Checking for any missing start times...")
  print(any(is.na(data$SampleStartTime)))
  
  print("Remove missing start times...")
  data <- data %>% dplyr::filter(!is.na(sample_datetime))
  # Removing DQL values lower than C and rejected results
  print("Removing DQL values below 'C' and 'rejected' results...")
  data <- filter(data,
                 QualifierAbbr %in% c("DQL=A", "DQL=B") | is.na(QualifierAbbr),
                 !Result_status %in% c("Rejected", "Provisional"))
  
  # Removing unnecessary columns
  data <-  data[, need_cols]
  
  # Remove all Dissolved Oxygen summary statistics from analysis except for 'Minimum'
  data <- data %>% dplyr::filter(!(Char_Name == "Dissolved oxygen" & Statistical_Base %in% c('7DADM', 'Maximum')))
  
  # Check for duplicate stations
  print("Checking for inconsistencies in station descriptions and Lat/Longs...")
  # data$Lat_DD <- round(data$Lat_DD, 5)
  # data$Long_DD <- round(data$Long_DD, 5)
  unique_stations_check <- data %>% dplyr::group_by(MLocID) %>% dplyr::summarise(n_unique_des = length(unique(StationDes)),
                                                                                 n_unique_lat = length(unique(Lat_DD)),
                                                                                 n_unique_long = length(unique(Long_DD)),
                                                                                 n_unique_huc8 = length(unique(HUC8))
  )
  print(any(unique_stations_check[, 2:5] > 1))
  
  if(any(unique_stations_check[, 2:5] > 1)){
    print("Assigning ODEQ station location info to duplicate stations...")
    dup_stations <- unique_stations_check[rowSums(unique_stations_check[, 2:5]) > 4, ]$MLocID
    odeq_stations <- unique(filter(data, MLocID %in% dup_stations, OrganizationID == "OREGONDEQ")[, c(1:2, 6:12)])
    
    data[data$MLocID %in% dup_stations & data$MLocID %in% odeq_stations$MLocID &
           data$OrganizationID != c("OREGONDEQ"), c(1:2, 6:12)] <- odeq_stations[match(
             data[data$MLocID %in% dup_stations & data$MLocID %in% odeq_stations$MLocID &
                    data$OrganizationID != c("OREGONDEQ"),]$MLocID, odeq_stations$MLocID), 1:9]
    
    print("Checking for inconsistencies in station descriptions and Lat/Longs...")
    unique_stations_check <- data %>% dplyr::group_by(MLocID) %>% dplyr::summarise(n_unique_des = length(unique(StationDes)),
                                                                                   n_unique_lat = length(unique(Lat_DD)),
                                                                                   n_unique_long = length(unique(Long_DD)),
                                                                                   n_unique_huc8 = length(unique(HUC8))
    )
    print(any(unique_stations_check[, 2:5] > 1))
  }
  # # Remove field replicates
  # data <- data[!grepl("Quality Control", data$Activity_Type), ]
  
  # Unit Standardization
  # convert ug/l to mg/l and deg F to deg C
  print("Converting units, ug/l -> mg/l and deg F -> deg C...")
  data <- data %>%
    dplyr::mutate(Result_Numeric = dplyr::case_when(Result_Unit == 'ug/l' ~ Result_Numeric * 0.001,
                                                    Result_Unit == 'deg F' ~ round((Result_Numeric - 32) * 5/9, 2),
                                                    TRUE ~ Result_Numeric),
                  Result = dplyr::case_when(Result_Unit == 'ug/l' ~ as.character(Result_Numeric),
                                            Result_Unit == 'deg F' ~ as.character(Result_Numeric),
                                            TRUE ~ Result),
                  Result_Unit = dplyr::case_when(Result_Unit == 'ug/l' ~ 'mg/l',
                                                 Result_Unit == 'deg F' ~ 'deg C',
                                                 TRUE ~ Result_Unit))
  
  # Check for duplicate samples
  print("Creating sample IDs...")
  data$sample_id <- paste(data$MLocID, data$Char_Name, data$sample_datetime, data$Statistical_Base, data$Method_Code, data$act_depth_height, sep = " ")
  print("Checking for duplicate samples...")
  if(any(duplicated(data$sample_id))){
    duplicated_ids <- data[duplicated(data$sample_id),"sample_id"]
    print(paste(length(duplicated_ids), "duplicate sample(s) found"))
    
    duplicates <- data[data$sample_id %in% duplicated_ids,]
    
    print("Categorizing duplication...")
    dup_checks <- duplicates %>% dplyr::group_by(sample_id) %>%
      dplyr::summarise(n_dups = n(),
                       sample_routine = dplyr::if_else("Field Msr/Obs" %in% Activity_Type & "Sample-Routine" %in% Activity_Type, 1, 0),
                       dup_results = dplyr::if_else(length(unique(Result)) == 1 & length(unique(QualifierAbbr)) == 1, 1, 0),
                       diff_DQL = dplyr::if_else(length(unique(QualifierAbbr)) == 1, 0, 1),
                       out_of_range = dplyr::if_else(unique(Char_Name) %in% c("Temperature, water", "Dissolved oxygen (DO)") &
                                                       (max(Result_Numeric, na.rm = TRUE) - min(Result_Numeric, na.rm = TRUE)) > 1.0, 1,
                                                     dplyr::if_else(unique(Char_Name) %in% c("pH") &
                                                                      (max(Result_Numeric, na.rm = TRUE) - min(Result_Numeric, na.rm = TRUE)) > 0.5, 1, 0)),
                       avg_result = mean(Result_Numeric, na.rm = TRUE)
      ) %>%
      dplyr::mutate(avg_calc = dplyr::if_else((sample_routine + dup_results + diff_DQL + out_of_range) == 0, 1, 0))
    
    print("Prioritizing field measurements over lab audit...")
    sample_routine_check <- dup_checks %>% dplyr::filter(sample_routine == 1)
    
    data <- data %>% dplyr::filter(!(sample_id %in% sample_routine_check$sample_id & Activity_Type == "Sample-Routine"))
    
    print("Resolving duplicate results...")
    same_result_check <- dup_checks %>% dplyr::filter(dup_results == 1, !sample_id %in% c(sample_routine_check$sample_id))
    same_result_data <- data %>% dplyr::filter(sample_id %in% unique(same_result_check$sample_id))
    same_result_data <- same_result_data[!duplicated(same_result_data$sample_id),]
    
    data <- data %>% dplyr::filter(!(sample_id %in% unique(same_result_check$sample_id)))
    
    print("Resolving different DQLs...")
    DQL_check <- dup_checks %>% dplyr::filter(diff_DQL == 1, !sample_id %in% c(sample_routine_check$sample_id,
                                                                               same_result_check$sample_id))
    DQL_check_data <- data %>% dplyr::filter(sample_id %in% unique(DQL_check$sample_id))
    DQL_resolve <- NULL
    for(i in unique(DQL_check$sample_id)){
      tmp <- DQL_check_data %>% dplyr::filter(sample_id == i)
      if("DQL=A" %in% tmp$QualifierAbbr){
        tmp <- tmp %>% dplyr::filter(QualifierAbbr == "DQL=A")
      } else {tmp <- tmp %>% dplyr::filter(QualifierAbbr == "DQL=B")}
      DQL_resolve <- dplyr::bind_rows(DQL_resolve, tmp)
    }
    
    data <- data %>% dplyr::filter(!(sample_id %in% unique(DQL_check$sample_id)))
    
    print("Resolving duplicate results outside of acceptable quality control range...")
    out_of_range <- dup_checks %>% dplyr::filter(out_of_range == 1, !sample_id %in% c(sample_routine_check$sample_id,
                                                                                      same_result_check$sample_id,
                                                                                      DQL_check$sample_id))
    
    data <- data %>% dplyr::filter(!(sample_id %in% out_of_range$sample_id))
    
    print("Calculating averages for remaining duplicates...")
    avg_ids <- dup_checks %>% dplyr::filter(avg_calc == 1)
    avg_result_data <- data %>% dplyr::filter(sample_id %in% unique(avg_ids$sample_id))
    avg_result_data <- avg_result_data[!duplicated(avg_result_data$sample_id),]
    avg_result_data[,"Result_Numeric"] <- avg_ids[match(avg_result_data$sample_id, avg_ids$sample_id), "avg_result"]
    
    data <- data %>% dplyr::filter(!(sample_id %in% avg_ids$sample_id))
    
    data <- dplyr::bind_rows(data, same_result_data, DQL_resolve, avg_result_data)
    
    duplicated_ids <- data[duplicated(data$sample_id),"sample_id"]
    print(paste("Remaining duplicate sample id(s):", paste(duplicated_ids, collapse = ", ")))
  } else {print("No duplicates found")}
  
  return(data)
}
