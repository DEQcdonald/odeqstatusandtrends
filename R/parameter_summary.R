#' Create parameter summary table for status and trends report
#'
#'
#' @param status Result from status_stns
#' @param sea_ken Result of Seasonal Kendall Analysis
#' @param stations Result of get_stations_AWQMS
#' @return Dataframe of summarized status and trends information
#' @export
#' @example parameter_summary(status = status_df, seaken = seasonal_kendall_df)

parameter_summary <- function(status, sea_ken, stations){
  st_stations <- unique(c(status$MLocID, sea_ken$MLocID))
  st_stations_info <- stations %>% dplyr::filter(MLocID %in% st_stations) %>%
    dplyr::select(AU_ID, GNIS_Name, MLocID, StationDes, Lat_DD, Long_DD, HUC8)

  param_sum <- merge(st_stations_info, status, by = "MLocID")
  param_sum <- merge(param_sum, sea_ken, by = c("MLocID","Char_Name"))
  param_sum <- dplyr::select(param_sum, AU_ID, GNIS_Name, Char_Name, MLocID, StationDes, status, trend, Lat_DD, Long_DD, HUC8)
  param_sum <- param_sum[order(param_sum[,1], param_sum[,3], param_sum[,4]),]
}