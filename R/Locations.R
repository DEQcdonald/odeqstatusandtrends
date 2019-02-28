#' Get stations in Oregon for status and trends analysis
#' 
#' Queries the ODEQ stations database to pull all available stations within a given shapefile.
#' @param polygon Shapefile of the area to query
#' @param stations.channel.name The name (in quotes) of your ODBC connection to the DEQLEAD-LIMS/Stations SQL repository. Defaults to "STATIONS".
#' @param exclude.tribal.lands Whether or not to exclude stations located on tribal lands. Defaults to TRUE.
#' @return A list of stations within a given shapefile.
#' @export
#' @examples 
#' get_stations_AWQMS(polygon = "your-shapefile-here", exclude.tribal.lands = TRUE, stations.channel.name = "STATIONS")

#@param parameters A list of parameters with which to filter the query. 

get_stations_AWQMS <- function(polygon, exclude.tribal.lands = TRUE, stations.channel.name = "STATIONS") {
  
  # Get Stations within station database
  stations.channel <- RODBC::odbcConnect(stations.channel.name)
  
  print("Retrieving all available stations from AWQMS...")
  
  s.time <- Sys.time()
  stations <- RODBC::sqlQuery(stations.channel, "SELECT * FROM VWStationsFinal", na.strings = "NA", stringsAsFactors=FALSE)
  e.time <- Sys.time()
  print(paste("This query took approximately", difftime(e.time, s.time, units = "secs"), "seconds"))
  
  # # Convert AWQMS characteristic names
  # AWQMS.parms <- AWQMS_Char_Names(parameters)
  # 
  # # Get stations
  # stations <- AWQMSdata::AWQMS_Stations(char = AWQMS.parms)
  
  # Clip stations to input polygon
  print("Clipping stations to your shapefile...")
  stations <- dplyr::filter(stations, MLocID %in% StationsInPoly(stations, polygon, outside = FALSE,
                                                                 id_col="MLocID", lat_col="Lat_DD", 
                                                                 lon_col="Long_DD", datum_col="Datum"))
  
  if(exclude.tribal.lands){
    
    print("Removing staions within tribal lands...")
          
    tribal.lands <- rgdal::readOGR(dsn = "//deqhq1/WQNPS/Agriculture/Status_and_Trend_Analysis/R_support_files", 
                                   layer = 'tl_2017_or_aiannh', integer64="warn.loss", verbose = FALSE)
    
    stations <- dplyr::filter(stations, MLocID %in% StationsInPoly(stations, tribal.lands, outside = TRUE,
                                                                   id_col="MLocID", lat_col="Lat_DD", 
                                                                   lon_col="Long_DD", datum_col="Datum"))
  }
  
  return(stations)
}

#' Get stations in Oregon for status and trends analysis
#' 
#' Queries the ODEQ stations database to pull all available stations within a given shapefile.
#' @param polygon Shapefile of the area to query
#' @param exclude.tribal.lands Whether or not to exclude stations located on tribal lands. Defaults to TRUE.
#' @return A list of stations within a given shapefile.
#' @export
#' @examples 
#' get_stations_NWIS(polygon = "your-shapefile-here", exclude.tribal.lands = TRUE)

get_stations_NWIS <- GetStations_AWQMS <- function(polygon, exclude.tribal.lands = TRUE) {
  
  # Get Stations within NWIS database
  print("Retrieving USGS station information for all of Oregon...")
  
  s.time <- Sys.time()
  stations <- dataRetrieval::readNWISdata(stateCd = "OR", 
                                          hasDataTypeCd="dv", 
                                          service="site", 
                                          siteType=c("ES", "LK", "SP", "ST", "ST-CA", "ST-DCH", "ST-TS", "WE"))
  e.time <- Sys.time()
  print(paste("This query took approximately", difftime(e.time, s.time, units = "secs"), "seconds"))
  
  # Clip stations to input polygon
  print("Clipping stations to your shapefile...")
  stations <- dplyr::filter(stations, site_no %in% StationsInPoly(stations=stations, polygon=polygon, outside = FALSE, 
                                                                 id_col="site_no", lat_col="dec_lat_va", 
                                                                 lon_col="dec_long_va", datum_col="dec_coord_datum_cd"))
  
  if(exclude.tribal.lands){
    
    print("Removing staions within tribal lands...")
          
          tribal.lands <- rgdal::readOGR(dsn = "//deqhq1/WQNPS/Agriculture/Status_and_Trend_Analysis/R_support_files", 
                                         layer = 'tl_2017_or_aiannh', integer64="warn.loss", verbose = FALSE)
          
          stations <- dplyr::filter(stations, site_no %in% StationsInPoly(stations, tribal.lands, outside = TRUE,
                                                                         id_col="site_no", lat_col="dec_lat_va", 
                                                                         lon_col="dec_long_va", datum_col="dec_coord_datum_cd"))
  }
  
  return(stations)
}

#' Clip stations to a polygon
#' 
#' Returns the stations located within a given shapefile, or outside if outside=TRUE.
#' @param stations Dataframe of stations with latitude, longitude, and source datum.
#' @param polygon Shapefile of the area to query
#' @param outside Set to true if you want stations located outside of the polygon instead. Default is FALSE.
#' @param id_col Quoted name of the station ID column
#' @param datum_col Quoted name of the datum column
#' @param lat_col Quoted name of the latitude column
#' @param lon_col Quoted name of the longitude column
#' @return A list of stations inside, or outside if outside=TRUE, of a given polygon.
#' @export
#' @examples 
#' StationsInPoly(stations = "result-of-GetStations()", polygon = "your-shapefile-here", outside=FALSE,
#' id_col = "MLocID", datum_col = "Datum", lat_col = "Lat_DD", lon_col = "Lon_DD")

StationsInPoly <- function(stations, polygon, outside=FALSE, id_col, datum_col, lat_col, lon_col) {
  
  # make a spatial object
  df.shp <- stations[,c(id_col, datum_col, lat_col, lon_col)]
  sp::coordinates(df.shp) <- c(lon_col, lat_col)
  
  # Datums to search for
  # NAD83 : EPSG:4269 <- This is assumed if it is not one of the other two
  # NAD27 : EPSG:4267
  # WGS84 : EPSG:4326
  
  df.nad83 <- df.shp[!grepl("NAD27|4267|WGS84|4326",toupper(df.shp@data[,datum_col])), ]
  df.nad27 <- df.shp[grepl("NAD27|4267",toupper(df.shp@data[,datum_col])), ]
  df.wgs84 <- df.shp[grepl("WGS84|4326",toupper(df.shp@data[,datum_col])), ]
  
  sp::proj4string(df.nad27) <- sp::CRS("+init=epsg:4267")
  sp::proj4string(df.nad83) <- sp::CRS("+init=epsg:4269")
  sp::proj4string(df.wgs84) <- sp::CRS("+init=epsg:4326")
  
  # convert to NAD 83
  if (nrow(df.nad27)>0) {
    df.nad27.nad83 <- sp::spTransform(df.nad27, sp::CRS("+init=epsg:4269")) 
    df.nad83 <- rbind(df.nad83, df.nad27.nad83)
  }
  
  if (nrow(df.wgs84)>0) {
    df.wgs84.nad83 <- sp::spTransform(df.wgs84, sp::CRS("+init=epsg:4269")) 
    df.nad83 <- rbind(df.nad83, df.wgs84.nad83)
  }
  
  poly.nad83 <- sp::spTransform(polygon, sp::CRS("+init=epsg:4269"))
  
  
  if(outside) {
    # stations outside polygon
    # df.out <- df.nad83[!stats::complete.cases(grDevices::over(df.nad83, poly.nad83)),]@data
    # stations.out <- unique(df.out$MLocID)
    stations.out <- unique(df.nad83[!df.nad83@data[,id_col] %in% unique(df.nad83[poly.nad83,]@data[,id_col]),]@data[,id_col])
    return(stations.out)
  } else {
    stations.in <- unique(df.nad83[poly.nad83,]@data[,id_col])
    return(stations.in)
  }
}
