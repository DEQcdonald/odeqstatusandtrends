#' Get data for water quality status and trends analysis
#' 
#' Queries the Oregon DEQ AWQMS database for water quality monitoring data fitting the given parameters. Requires permissions to the VW_AWQMS_Results 
#' view in the AWQMS repository. This query is specific to the methods used in status and trends reporting. For a more comprehensive AWQMS query, see 
#' the AWQMS package by Travis Pritchard.
#' @param parameters A list of parameters to include in the query.
#' @param stations Stations dataframe from GetStations().
#' @param start.date The earliest date to include in the query. "%Y-%m-%d"
#' @param end.date The latest date to include in the query. "%Y-%m-%d"
#' @param awqms.channel.name The name in quotes of the AWQMS ODBC connection. Defaults to "AWQMS".
#' @return A dataframe of all available data within AWQMS that fit the query.
#' @export
#' @examples
#' GetData(parameters = c("parameter1", "parameter2"), 
#' stations = result-of-GetStations(), 
#' start.date = "2010-01-01", end.date = "2019-01-01", 
#' awqms.channel.name = "AWQMS")

GetData <- function(parameters = NULL, stations, start.date, end.date, awqms.channel.name = "AWQMS") {

  # Convert characteristic names
  AWQMS.parms <- AWQMS_Char_Names(parameters)
  
  #### Define sample media to query ####
  sample.media <- 'Water'
  print(paste('Querying the AWQMS database for data at', length(stations$MLocID), 'stations related to:', paste(parameters, collapse = ", ")))
  s.time <- Sys.time()
  data.AWQMS <- AWQMSdata::AWQMS_Data(startdate = start.date, 
                                      enddate = end.date, 
                                      char = AWQMS.parms, 
                                      media = sample.media,
                                      crit_codes = TRUE, 
                                      station = stations$MLocID)
  e.time <- Sys.time()
  print(paste("This query took approximately", difftime(e.time, s.time, units = "secs"), "seconds."))
  
  data.AWQMS <- data.AWQMS %>% filter(MonLocType %in% c("River/Stream", "Lake", "Other-Surface Water", ""))
  
  data.AWQMS <- merge(data.AWQMS, stations[, c("MLocID", "Datum")], by="MLocID", all.x = TRUE, all.y = FALSE)
  
  # #### connect to element and get data (must set up ODBC connection first) ####
  # channel <- odbcConnect(awqms.channel.name)
  # table <- "VW_AWQMS_Results"
  # 
  # site.type = "'River/Stream', 'Lake', 'Other-Surface Water', 'Reservoir'"
  # 
  # #### Create SQL query ####
  # myQuery <- paste0("SELECT * FROM VW_AWQMS_Results WHERE MLocID IN ('", paste(stations, collapse = "', '"),
  #                   "') AND Char_Name IN ('", paste(AWQMS.parms, collapse = "', '"),
  #                   "') AND SampleStartDate BETWEEN '", start.date, "' AND '", end.date,
  #                   "' AND SampleMedia='", sample.media,
  #                   "' AND MonLocType IN (", site.type, ")"
  # )
  # 
  # print(paste('Querying', length(stations), 'stations from AWQMS for water quality data related to:', paste(parameters, collapse = ", ")))
  # 
  # #### Pass the SQL query to AWQMS ####
  # sTime <- Sys.time()
  # AWQMS.data <- sqlQuery(channel, myQuery, errors = FALSE)
  # eTime <- Sys.time()
  # print(paste("This query took approximately", difftime(eTime, sTime, units = "secs"), "seconds."))
  
  return(data.AWQMS)
}
