#' Get data for water quality status and trends analysis
#'
#' Queries the Oregon DEQ AWQMS database for water quality monitoring data fitting the given parameters. Requires permissions to the VW_AWQMS_Results
#' view in the AWQMS repository. This query is specific to the methods used in status and trends reporting. For a more comprehensive AWQMS query, see
#' the AWQMS package by Travis Pritchard.
#' @param parameters A list of parameters to include in the query
#' @param stations_AWQMS Stations dataframe from get_stations_AWQMS()
#' @param start.date The earliest date to include in the query. "%Y-%m-%d"
#' @param end.date The latest date to include in the query. "%Y-%m-%d"
#' @param query_nwis Logical. Should the function query the USGS NWIS database.
#' @param stations_NWIS Stations dataframe from get_stations_NWIS()
#' @param awqms.channel.name The name in quotes of the AWQMS ODBC connection. Defaults to "AWQMS".
#' @return A dataframe of all available data within AWQMS that fit the query.
#' @export
#' @examples
#' GetData(parameters = c("parameter1", "parameter2"),
#' stations_AWQMS = result-of-get_stations_AWQMS(),
#' stations_NWIS = result-of-get_stations_NWIS(),
#' start.date = "2010-01-01", end.date = "2019-01-01",
#' awqms.channel.name = "AWQMS")

GetData <- function(parameters = NULL, stations_AWQMS, start.date, end.date, query_nwis = FALSE, stations_NWIS, awqms.channel.name = "AWQMS") {

  # Convert characteristic names
  AWQMS.parms <- AWQMS_Char_Names(parameters)

  #### Define sample media to query ####
  sample.media <- 'Water'

  print(paste('Querying the AWQMS database for data at', length(stations_AWQMS$MLocID), 'stations related to:', paste(parameters, collapse = ", ")))

  s.time <- Sys.time()
  data_AWQMS <- AWQMSdata::AWQMS_Data(startdate = start.date,
                                      enddate = end.date,
                                      char = AWQMS.parms,
                                      media = sample.media,
                                      crit_codes = TRUE,
                                      station = stations_AWQMS$MLocID)
  e.time <- Sys.time()
  print(paste("This query took approximately", difftime(e.time, s.time, units = "secs"), "seconds."))

  # Include only relevant monitoring location types
  data_AWQMS <- data_AWQMS %>% filter(MonLocType %in% c("River/Stream", "Lake", "Other-Surface Water", ""))

  # Attach location datum info to observations
  data_AWQMS <- merge(data_AWQMS, stations_AWQMS[, c("MLocID", "Datum")], by="MLocID", all.x = TRUE, all.y = FALSE)

  if(query_nwis){
    usgs_stations <- unique(stations_NWIS$site_no)
    if("Temperature, water" %in% AWQMS.parms) {
      usgs_parms <- c("00010")
    }
    if("Dissolved oxygen (DO)" %in% AWQMS.parms) {
      usgs_parms <- c(usgs_parms, "00300", "00301")
    }

    print(paste('Querying the NWIS database for data at', length(usgs_stations), 'stations related to:',
                paste(parameterCdFile[parameterCdFile$parameter_cd %in% usgs_parms,"parameter_nm"], collapse = ", ")))

    s.time <- Sys.time()
    data_NWIS <- dataRetrieval::readNWISdata(sites = usgs_stations,
                                             startDt = start.date,
                                             endDt = end.date,
                                             siteType = c("ES", "LK", "SP", "ST", "ST-CA", "ST-DCH", "ST-TS", "WE"),
                                             parameterCd = usgs_parms)
    e.time <- Sys.time()
    print(paste("This query took approximately", difftime(e.time, s.time, units = "secs"), "seconds."))

    data_NWIS <- merge(data_NWIS, attributes(data_NWIS)$siteInfo, by = c("site_no", "agency_cd"), all.x = TRUE, all.y = FALSE)
    data_NWIS <- dplyr::rename(data_NWIS, Result_Numeric = X_00010_00001,
                               StationDes = station_nm,  OrganizationID = agency_cd, MLocID = site_no, SampleStartDate = dateTime,
                               SampleStartTZ = tz_cd, Lat_DD = dec_lat_va, Long_DD = dec_lon_va, Datum = srs, MonLocType = siteTypeCd,
                               HUC8 = hucCd)
    data_NWIS <- dplyr::mutate(data_NWIS, Statistical_Base = "Maximum", Tesult_Type = "Calculated", Result_Unit = "deg C",
                               Char_Name = "Temperature, water", Activity_Type = "NWIS", SampleMedia = "Water",
                               SampleSubmedia = "Surface Water", Unit_UID = 246)
    data_NWIS$SampleStartDate <- as.character(data_NWIS$SampleStartDate)

    data_combined <- bind_rows(data_AWQMS, data_NWIS)
  }

  return(data_AWQMS)
}
