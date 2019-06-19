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

  data <- data[, c('MLocID', 'StationDes', 'OrganizationID', 'Org_Name', 'Lat_DD', 'Long_DD', 'ELEV_Ft', 'Datum', 'HUC8', 'AU_ID', 'Reachcode', 'Char_Name', 'sample_datetime', 'Result',
                   'Result_Numeric', 'Result_Operator', 'Result_Unit', 'Statistical_Base', 'QualifierAbbr', 'Method_Code', 'Activity_Type', 'act_id', 'MRLValue',
                   'Result_status', "FishCode", "SpawnCode", "WaterTypeCode", "WaterBodyCode", "BacteriaCode", "DO_code", "ben_use_code",
                   "pH_code", "DO_SpawnCode")]

  # Remove all Dissolved Oxygen summary statistics from analysis except for 'Minimum'
  data <- data %>% dplyr::filter(!(Char_Name == "Dissolved oxygen" & Statistical_Base %in% c('7DADM', 'Maximum')))

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
  data$sample_id <- paste(data$MLocID, data$Char_Name, data$sample_datetime, data$Statistical_Base, data$Method_Code, sep = " ")
  print("Checking for duplicate samples...")
  if(any(duplicated(data$sample_id))){
    duplicated_ids <- data[duplicated(data$sample_id),"sample_id"]
    print(paste(length(duplicated_ids), "duplicate sample(s) found"))
    print("Prioritizing field measurements over lab audit...")
    duplicates <- data[data$sample_id %in% duplicated_ids,]
    n <- 0
    for(i in unique(duplicates$sample_id)){
      dup_df <- duplicates %>% filter(sample_id == i)
      if(sum(dup_df$Activity_Type == "Field Msr/Obs") == 1 & sum(dup_df$Activity_Type == "Sample-Routine") == 1){
        data <- data %>% filter(!(sample_id == i & Activity_Type == "Sample-Routine"))
        n <- n + 1
      }
    }
    print(paste("Removed", n, "lab audit result(s) where field measures were available"))
    print(paste("Remaining duplicate sample id(s):", paste(duplicated_ids <- data[duplicated(data$sample_id),"sample_id"], collapse = ", ")))
  } else {print("No duplicates found")}

  # Removing DQL values lower than C and rejected results
  print("Removing DQL values below 'C' and 'rejected' results...")
  data <- filter(data,
                 QualifierAbbr %in% c("DQL=A", "DQL=B", "DQL=C") | is.na(QualifierAbbr),
                 Result_status != "Rejected")

  return(data)
}
