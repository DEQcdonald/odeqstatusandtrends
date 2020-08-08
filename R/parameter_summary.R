#' Create parameter summary table for status and trends report
#'
#'
#' @param status Result from status_stns
#' @param sea_ken Result of Seasonal Kendall Analysis
#' @param stations Result of get_stations_AWQMS
#' @return Dataframe of summarized status and trends information
#' @export
#' @examples parameter_summary(status = status_df, seaken = seasonal_kendall_df)

parameter_summary_by_station <- function(status, sea_ken, stations){
  st_stations <- unique(status$MLocID)
  
  if(!is.null(sea_ken)){
    st_stations <- unique(c(status$MLocID, sea_ken$MLocID))
  }
  
  st_stations_info <- stations %>% dplyr::filter(MLocID %in% st_stations) %>%
    dplyr::select(AU_Name, AU_ID, MLocID, StationDes, Lat_DD, Long_DD, HUC8, HUC8_Name)
  
  param_sum <- merge(st_stations_info, status, by = "MLocID")
  
  if(!is.null(sea_ken)){
    param_sum <- merge(param_sum, sea_ken, by = c("MLocID", "Char_Name"), all.x = TRUE)
  } else {param_sum$trend <- "Insufficient Data"}
  
  param_sum <- dplyr::select(param_sum, AU_ID, AU_Name, Char_Name, MLocID, StationDes,
                             # rev(colnames(status)[3:length(colnames(status))]),
                             colnames(status)[3:length(colnames(status))], trend, Lat_DD, Long_DD, HUC8, HUC8_Name)
  param_sum <- param_sum[order(param_sum[,1], param_sum[,3], param_sum[,4]),]
  param_sum[is.na(param_sum$trend), "trend"] <- "Insufficient Data"
  return(param_sum)
}

#' Create parameter summary table for status and trends report
#'
#'
#' @param status Result from status_stns
#' @param sea_ken Result of Seasonal Kendall Analysis
#' @param stations Result of get_stations_AWQMS
#' @return Dataframe of summarized status and trends information
#' @export
#' @examples parameter_summary(status = status_df, seaken = seasonal_kendall_df)

parameter_summary_by_au <- function(status, sea_ken, stations){
  st_stations <- unique(status$MLocID)
  
  if(!is.null(sea_ken)){
    st_stations <- unique(c(status$MLocID, sea_ken$MLocID))
  }
  
  st_stations_info <- stations %>% dplyr::filter(MLocID %in% st_stations) %>%
    dplyr::select(AU_ID, AU_Name, MLocID, StationDes, Lat_DD, Long_DD, HUC8, HUC8_Name) %>% 
    dplyr::group_by(AU_ID, AU_Name, MLocID, StationDes, Lat_DD, Long_DD) %>% 
    dplyr::summarise(HUC8 = first(HUC8), HUC8_Name = first(HUC8_Name))
  
  param_sum <- merge(st_stations_info, status, by = "MLocID")
  
  if(!is.null(sea_ken)){
    param_sum <- merge(param_sum, sea_ken, by = c("MLocID", "Char_Name"), all.x = TRUE)
  } else {param_sum$trend <- "Insufficient Data"}
  
  param_sum <- dplyr::select(param_sum, AU_ID, AU_Name, Char_Name, MLocID, StationDes,
                             # rev(colnames(status)[3:length(colnames(status))]),
                             colnames(status)[3:length(colnames(status))], trend, Lat_DD, Long_DD, HUC8, HUC8_Name)
  param_sum <- param_sum[order(param_sum[,1], param_sum[,3], param_sum[,4]),]
  param_sum[is.na(param_sum$trend), "trend"] <- "Insufficient Data"
  
  assess_sum <- param_sum %>% 
    dplyr::select(-MLocID, -StationDes, -trend, -Lat_DD, -Long_DD) %>%
    dplyr::group_by(AU_ID, AU_Name, Char_Name) %>%
    dplyr::summarise_all(function(x){
      y <- dplyr::if_else(any(x %in% "Not Attaining"),
                          "Not Attaining",
                          dplyr::if_else(any(x %in% "Attaining"),
                                         "Attaining",
                                         dplyr::if_else(any(x %in% "Insufficient Data"),
                                                        "Insufficient Data",
                                                        "Unassessed"
                                         )
                          )
      )
      return(y)
    }) %>% dplyr::ungroup()
  AU_HUC <- st_stations_info %>% dplyr::group_by(AU_ID) %>% dplyr::summarise(HUC_Name = first(HUC8_Name), HUC8 = first(HUC8))
  
  assess_sum <- merge(assess_sum, AU_HUC, by="AU_ID", all.x = TRUE, all.y = FALSE)
  
  return(assess_sum)
}
