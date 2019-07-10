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
  data[is.na(data$SampleStartTime), "SampleStartTime"] <- "00:00:00.000000"
  data$sample_datetime <- paste(data$SampleStartDate, data$SampleStartTime)
  data$sample_datetime <- as.POSIXct(data$sample_datetime, format = '%Y-%m-%d %H:%M:%S')

  print("Checking for any missing start times...")
  print(any(is.na(data$SampleStartTime)))

  # Removing DQL values lower than C and rejected results
  print("Removing DQL values below 'C' and 'rejected' results...")
  data <- filter(data,
                 QualifierAbbr %in% c("DQL=A", "DQL=B", "DQL=C") | is.na(QualifierAbbr),
                 Result_status != "Rejected")

  # Removing unnecessary columns
  data <- data[, c('MLocID', 'StationDes', 'OrganizationID', 'Project1', 'Org_Name', 'Lat_DD', 'Long_DD', 'ELEV_Ft', 'Datum', 'HUC8', 'AU_ID', 'Reachcode', 'Char_Name', 'sample_datetime', 'Result',
                   'Result_Numeric', 'Result_Operator', 'Result_Unit', 'Statistical_Base', 'Result_Depth', 'QualifierAbbr', 'Method_Code', 'Activity_Type', 'act_id', 'MRLValue',
                   'Result_status', "FishCode", "SpawnCode", "WaterTypeCode", "WaterBodyCode", "BacteriaCode", "DO_code", "ben_use_code",
                   "pH_code", "DO_SpawnCode")]

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

  # Check for duplicate samples
  print("Creating sample IDs...")
  data$sample_id <- paste(data$MLocID, data$Char_Name, data$sample_datetime, data$Statistical_Base, data$Method_Code, data$Result_Depth, sep = " ")
  print("Checking for duplicate samples...")
  if(any(duplicated(data$sample_id))){
    duplicated_ids <- data[duplicated(data$sample_id),"sample_id"]
    print(paste(length(duplicated_ids), "duplicate sample(s) found"))

    duplicates <- data[data$sample_id %in% duplicated_ids,]

    print("Categorizing duplication...")
    dup_checks <- duplicates %>% dplyr::group_by(sample_id) %>%
      dplyr::summarise(sample_routine = if_else("Field Msr/Obs" %in% Activity_Type & "Sample-Routine" %in% Activity_Type, 1, 0),
                       dup_results = if_else(length(unique(Result)) == 1 & length(unique(QualifierAbbr)) == 1, 1, 0))

    print("Prioritizing field measurements over lab audit...")
    sample_routine_check <- dup_checks %>% dplyr::filter(sample_routine == 1)

    print("Checking for duplicate results...")
    same_result_check <- dup_checks %>% dplyr::filter(dup_results == 1)
    same_result_data <- data %>% dplyr::filter(sample_id %in% unique(same_result_check$sample_id))
    same_result_data <- same_result_data[!duplicated(same_result_data$sample_id),]

    data <- data %>% dplyr::filter(!(sample_id %in% sample_routine_check$sample_id & Activity_Type == "Sample-Routine"),
                                   !(sample_id %in% unique(same_result_check$sample_id)))

    data <- bind_rows(data, same_result_data)

    print(paste("Removed", n, "lab audit result(s) where field measures were available"))
    print(paste("Remaining duplicate sample id(s):", paste(duplicated_ids <- data[duplicated(data$sample_id),"sample_id"], collapse = ", ")))
  } else {print("No duplicates found")}

  return(data)
}
