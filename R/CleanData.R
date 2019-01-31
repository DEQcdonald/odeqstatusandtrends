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
  # Add datetime columns and remove unused variables
  data$sample_datetime <- paste(data$SampleStartDate, data$SampleStartTime)
  data$sample_datetime <- as.POSIXct(data$sample_datetime, format = '%Y-%m-%d %H:%M:%S')
  
  print("Checking for any missing start times...")
  print(any(is.na(data$SampleStartTime)))
  
  data <- data[, c('MLocID', 'StationDes', 'OrganizationID','Lat_DD', 'Long_DD', 'Datum', 'HUC8', 'Char_Name', 'sample_datetime', 'Result',
                   'Result_Numeric', 'Result_Operator', 'Result_Unit', 'Statistical_Base', 'QualifierAbbr', 'Activity_Type', 'MRLValue',
                   'Result_status', "FishCode", "SpawnCode", "WaterTypeCode", "WaterBodyCode", "BacteriaCode", "DO_code", "ben_use_code", 
                   "pH_code", "DO_SpawnCode")]
  
  # Remove all Dissolved Oxygen summary statistics from analysis except for 'Minimum'
  data <- data %>% dplyr::filter(!(Char_Name == "Dissolved oxygen" & Statistical_Base %in% c('7DADM', 'Maximum', 'Mean', '7DADMean', '7DADMin', '30DADMean')))
  
  # Check for duplicate stations
  print("Checking for inconsistencies in station descriptions and Lat/Longs...")
  unique_stations_check <- data %>% dplyr::group_by(MLocID) %>% dplyr::summarise(n_unique_des = length(unique(StationDes)),
                                                                                 n_unique_lat = length(unique(Lat_DD)),
                                                                                 n_unique_long = length(unique(Long_DD)),
                                                                                 n_unique_huc8 = length(unique(HUC8))
                                                                                 )
  print(any(unique_stations_check[, 2:5] > 1))
  
  # # Remove field replicates
  # data <- data[!grepl("Quality Control", data$Activity_Type), ]
  
  # Check for duplicate samples
  data$sample_id <- paste(data$MLocID, data$Char_Name, data$sample_datetime, data$Statistical_Base, sep = " ")
  print("Checking for duplicate samples...")
  if(any(duplicated(data$sample_id))){
    print("Duplicate samples found")
    print(paste("Duplicated sample id(s):", paste(data[duplicated(data$sample_id),"sample_id"], sep = ", ")))
  } else {print("No duplicates found")}
  
  # # Perform censored data modifications for results with >, <, etc.
  # data <- data %>%
  #   # Get lowest criteria value to set censored results
  #   mutate(Result_cen = ifelse(UQ(resqual) == "=", as.numeric(UQ(res)),
  #                               ifelse(UQ(resqual) == ">", as.numeric(UQ(res)), 
  #                                      ifelse(UQ(resqual) == "<", ifelse(UQ(res) > as.numeric(UQ(crit)), 0.5 * as.numeric(UQ(crit)) , 0.5 * as.numeric(UQ(res)) ), "ER" ))))
  # data <- data %>% 
  #   mutate(Result_cen = if(Result_Operator %in% c("=", ">")){
  #     as.numeric(Result_Numeric)
  #   } else if(Result_Operator == "<") {
  #       if(Result_Numeric > 
  #     }
  
  # Removing DQL values lower than C and rejected results
  print("Removing DQL values below 'C' and 'rejected' results...")
  data <- filter(data, 
                 QualifierAbbr %in% c("DQL=A", "DQL=B", "DQL=C") | is.na(QualifierAbbr),
                 Result_status != "Rejected")
  
  return(data)
}
