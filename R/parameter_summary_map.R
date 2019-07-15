#' Create parameter summary map for status and trends report
#'
#'
#' @param param_summary Parameter summary table from parameter_summary()
#' @param area Shapefile for assessment areas
#' @return map of status and trends results
#' @export
#' @examples parameter_summary_map(param_summary = parameter_summary_df)

parameter_summary_map <- function(param_summary, area){

# Set up shapefiles for map -----------------------------------------------
  print("Processing shapefiles...")
  query <- paste0("SELECT * FROM AssessmentUnits_OR_Lines WHERE AU_ID IN ('",
                  paste(unique(param_summary$AU_ID), collapse = "', '"), "')")

  assessment_units <- sf::st_read(
    dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/Assessment.gdb",
    layer = "AssessmentUnits_OR_Lines",
    query = query, stringsAsFactors = FALSE
  )

  wql_streams <- sf::st_read(
    dsn = "//deqhq1/WQNPS/Agriculture/Status_and_Trend_Analysis/R_support_files",
    layer = "WQL_Streams_2012",
    query = paste0("SELECT * FROM WQL_Streams_2012 WHERE HUC_4TH_CO IN ('",
                   paste(unique(param_summary$HUC8), collapse = "', '"), "')"),
    stringsAsFactors = FALSE
  )
  wql_streams$Char_Name <- unlist(sapply(wql_streams$POLLUTANT, AWQMS_Char_Names, USE.NAMES = FALSE))

  assessment_units <- sf::st_zm(assessment_units, what = "ZM")
  wql_streams <- sf::st_zm(wql_streams, what = "ZM")

  st_crs(assessment_units)
  assessment_units <- st_transform(assessment_units, 4326)
  assessment_units <- assessment_units[,c("AU_ID", "AU_Name")] %>% filter(AU_ID != "99")
  st_crs(wql_streams)
  wql_streams <- st_transform(wql_streams, 4326)
  wql_streams <- filter(wql_streams[, c("STREAM_NAM", "SEGMENT_ID", "SEASON", "Char_Name", "LISTING_ST", "TMDL_INFO")], Char_Name %in% unique(param_summary$Char_Name))
  wql_streams <- wql_streams[lapply(wql_streams$`_ogr_geometry_`, length) != 0,]
  wql_streams$TMDL_INFO <- vapply(strsplit(wql_streams$TMDL_INFO, "<a"), `[`, 1, FUN.VALUE=character(1))

  if(!any(class(area) == "sf")){
    area <- sf::st_as_sf(area)
  }

  st_crs(area)
  area <- st_transform(area, 4326)

# Create functions for mapping --------------------------------------------------------

  au_colors <- param_summary %>% group_by(AU_ID, Char_Name) %>%
    summarise(color = if_else(all(is.na(status)),
                              "grey",
                              if_else(any(status == "Not Attaining"),
                                      "orange",
                                      "green")
                              )
              )

  param_summary$color <- if_else(is.na(param_summary$status),
                                 "gray",
                                 if_else(param_summary$status == "Attaining",
                                         "green",
                                         "orange"))
  param_summary$icon <- sapply(param_summary$trend,
                               function(x){
                                 if(x == "Improving"){
                                   "glyphicon-arrow-up"
                                 } else if(x == "Degrading"){
                                   "glyphicon-arrow-down"
                                 } else if(x == "No Sig Trend"){
                                   "glyphicon-minus"
                                 } else if(x == "Steady") {
                                   "glyphicon-arrow-right"
                                 } else {"glyphicon-none"}
                               }
                               )
  # function to pull the selected parameter's status and trend and create a popup table for the station.
  # This function is called on the click of a station marker in the parameter summary map.
  popupTable <- function(station = NULL, AU = NULL, param){
    if(!is.null(station)){
      table <- kable(filter(param_summary[, c("MLocID", "Char_Name", "status", "trend")],
                            MLocID == station, Char_Name == param) %>% select(status, trend),
                     format = "html", row.names = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    }
    if(!is.null(AU)){
      table <- kable(filter(param_summary[, c("AU_ID", "Char_Name", "MLocID", "status", "trend")],
                            AU_ID == AU, Char_Name == param) %>% select(MLocID, status, trend),
                     format = "html", row.names = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
    }
    return(table)
  }

  WQLpopupTable <- function(seg_ID = NULL, param = NULL){
    if(!is.null(seg_ID)){
      table <- kable(filter(wql_streams_data,
                            SEGMENT_ID == seg_ID, Char_Name == param) %>% select(Char_Name, LISTING_ST, SEASON, TMDL_INFO) %>% unique(),
                     format = "html", row.names = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE)
    }
    return(table)
  }

# Create parameter summary map --------------------------------------------

  print("Creating Map...")

  wql_streams_data <- sf::st_drop_geometry(wql_streams)

  map <- leaflet() %>% addTiles() %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
    addWMSTiles(baseUrl = 'https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2016_Land_Cover_L48/wms?',
                group = "Land Cover (NLCD 2016)",
                layers = "NLCD_2016_Land_Cover_L48",
                options = WMSTileOptions(version = '1.3.0',
                                         format = 'image/png',
                                         transparent = TRUE)) %>%
    addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer",
                group = "Hydrography",
                options = WMSTileOptions(format = "image/png",
                                         transparent = TRUE),
                layers = "0") %>%
    addPolygons(data = area, fill = FALSE, group = "Assessment Area", label = "Assessment Area")

    # if(nrow(wql_streams_data) > 0){
      # map <- map %>%
      # addPolylines(data = wql_streams,
      #              opacity = 0.7,
      #              weight = 2,
      #              color = "blue",
      #              # popup = ~paste0("<b>", STREAM_NAM,
      #              #                 "<br>Parameter:</b> ", Char_Name,
      #              #                 "<br><b>Listing:</b> ", LISTING_ST),
      #              popup = ~paste0("<b>", STREAM_NAM, "<br></b>",
      #                              sapply(SEGMENT_ID, WQLpopupTable, param = i, USE.NAMES = FALSE)),
      #              popupOptions = popupOptions(maxWidth = 600),
      #              group = "WQL Streams"
      # )
    # } else {print(paste("No water quality limited streams for", i))}

  for(i in unique(param_summary$Char_Name)){
    print(paste("Adding layer for", i))
    au_data <- filter(assessment_units[, c("AU_ID", "AU_Name")], AU_ID %in% unique(param_summary[param_summary$Char_Name == i,]$AU_ID))
    au_data <- merge(au_data, filter(au_colors, Char_Name == i)[,c("AU_ID", "color")], by = "AU_ID")
    wql_streams_tmp <- filter(wql_streams, Char_Name == i)

    if(nrow(wql_streams_data) > 0){
      map <- map %>%
        addPolylines(data = wql_streams_tmp,
                     opacity = 0.7,
                     weight = 2,
                     color = "blue",
                     # popup = ~paste0("<b>", STREAM_NAM,
                     #                 "<br>Parameter:</b> ", Char_Name,
                     #                 "<br><b>Listing:</b> ", LISTING_ST),
                     popup = ~paste0("<b>", STREAM_NAM,
                                     "<br>Parameter:</b> ", Char_Name,
                                     "<br></b>",
                                     sapply(SEGMENT_ID, WQLpopupTable, param = i, USE.NAMES = FALSE)),
                     popupOptions = popupOptions(maxWidth = 600),
                     group = i
        )
    } else {print(paste("No water quality limited streams for", i))}

     map <- map %>%
      addPolylines(data = au_data,
                   opacity = 1,
                   weight = 3,
                   color = ~color,
                   popup = ~paste0("<b>", AU_Name, "<br>AU:</b> ", AU_ID,
                                   "<br><b>Parameter:</b> ", i, "<br>",
                                   sapply(AU_ID, popupTable, station = NULL, param = i, USE.NAMES = FALSE)
                   ),
                   popupOptions = popupOptions(maxWidth = 600),
                   group = i
      )

     map <- map %>%
      addAwesomeMarkers(data = filter(param_summary, Char_Name == i),
                        lat = ~Lat_DD,
                        lng = ~Long_DD,
                        icon = awesomeIcons(icon = ~icon,
                                            iconColor = 'black',
                                            library = 'glyphicon',
                                            markerColor = ~color),
                        label = ~MLocID,
                        popup = ~paste0("<b>", StationDes, "<br>ID:</b> ", MLocID,
                                        "<br><b>AU:</b> ", AU_ID,
                                        "<br><b>Parameter:</b> ", i, "<br>",
                                        sapply(MLocID, popupTable, AU = NULL, param = i, USE.NAMES = FALSE)),
                        popupOptions = popupOptions(maxWidth = 600),
                        labelOptions = list(className = "stationLabels", noHide = T, permanent = T, interactive = T, direction = "auto",
                                            offset = c(-10,-25), opacity = 0.9, textsize = "14px", sticky = TRUE),

                        group = i
      )

  }

  map <- map %>%
    addLayersControl(baseGroups = unique(param_summary$Char_Name),
                                  overlayGroups = c("Assessment Area", "World Imagery", "Hydrography", "Land Cover (NLCD 2016)")) %>%
    hideGroup(c(unique(param_summary$Char_Name)[-1],
                paste(unique(param_summary$Char_Name), "Labels"),
                "World Imagery", "Hydrography", "Land Cover (NLCD 2016)")) %>%
    addEasyButton(easyButton(
      icon = "fa-globe",
      title = "Zoom to assessment area",
      onClick = JS("function(btn, map){
                var groupLayer = map.layerManager.getLayerGroup('Assessment Area');
                map.fitBounds(groupLayer.getBounds());
                 }"))) %>%
    addEasyButton(easyButton(
      icon = "fa-map-signs",
      title = "Toggle Station ID labels",
      onClick = JS("function(btn, map){
    var elements = document.getElementsByClassName('stationLabels');
    var index;

    elements = elements.length ? elements : [elements];
  for (index = 0; index < elements.length; index++) {
    element = elements[index];

    if (isElementHidden(element)) {
      element.style.display = '';

      // If the element is still hidden after removing the inline display
      if (isElementHidden(element)) {
        element.style.display = 'block';
      }
    } else {
      element.style.display = 'none';
    }
  }
  function isElementHidden (element) {
    return window.getComputedStyle(element, null).getPropertyValue('display') === 'none';
  }
               }"
      )
    )) %>% leaflet.extras::addSearchFeatures(targetGroups = unique(param_summary$Char_Name))

# map <- browsable(
#     tagList(list(
#       tags$head(
#         tags$style(
#           ".leaflet-popup {
# margin-bottom: 55px;
# }
#
# .leaflet-popup-content-wrapper {
# background: white;
# offset: relative;
# color: black;
# padding: 5px;
# }
#
# .leaflet-popup-content {
#           font-style: bold;
# font-size: 15px;
# margin: auto;
# }"
#         )
#       ),
#       map
#     ))
# )

  return(map)
}
