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
  st_stations <- unique(c(status$MLocID, sea_ken$MLocID))
  st_stations_info <- stations %>% dplyr::filter(MLocID %in% st_stations) %>%
    dplyr::select(AU_Name, AU_ID, MLocID, StationDes, Lat_DD, Long_DD, HUC8, HUC8_Name)

  param_sum <- merge(st_stations_info, status, by = "MLocID")
  param_sum <- merge(param_sum, sea_ken, by = c("MLocID", "Char_Name"), all.x = TRUE)
  param_sum <- dplyr::select(param_sum, AU_Name, AU_ID, Char_Name, MLocID, StationDes,
                             rev(colnames(status)[3:length(colnames(status))]), trend, Lat_DD, Long_DD, HUC8, HUC8_Name)
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
  st_stations <- unique(c(status$MLocID, sea_ken$MLocID))
  st_stations_info <- stations %>% dplyr::filter(MLocID %in% st_stations) %>%
    dplyr::select(AU_ID, AU_Name, MLocID, StationDes, Lat_DD, Long_DD, HUC8, HUC8_Name)

  param_sum <- merge(st_stations_info, status, by = "MLocID")
  param_sum <- merge(param_sum, sea_ken, by = c("MLocID", "Char_Name"), all.x = TRUE)
  param_sum <- dplyr::select(param_sum, AU_ID, AU_Name, Char_Name, MLocID, StationDes,
                             rev(colnames(status)[3:length(colnames(status))]), trend, Lat_DD, Long_DD, HUC8, HUC8_Name)
  param_sum <- param_sum[order(param_sum[,1], param_sum[,3], param_sum[,4]),]
  param_sum[is.na(param_sum$trend), "trend"] <- "Insufficient Data"

  assess_sum <- param_sum %>% dplyr::select(-MLocID, -StationDes, -trend, -Lat_DD, -Long_DD, -HUC8, -HUC8_Name) %>%
    dplyr::group_by(AU_ID, Char_Name) %>%
    dplyr::summarise_all(function(x){
      y <- if_else(any(x == "Not Attaining"),
                   "Not Attaining",
                   if_else(any(x == "Attaining"),
                           "Attaining",
                           if_else(any(x == "Insufficient Data"),
                                   "Insufficient Data",
                                   "Unassessed"
                           )
                   )
      )
      return(y)
    }) %>% ungroup()

  return(assess_sum)
}
