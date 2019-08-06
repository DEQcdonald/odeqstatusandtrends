#' Create parameter summary map for status and trends report
#'
#'
#' @param param_summary Parameter summary table from parameter_summary()
#' @param assess_summary Assessment summary table from parameter_summary()
#' @param area Shapefile for assessment areas
#' @return map of status and trends results
#' @export
#' @examples parameter_summary_map(param_summary = parameter_summary_df)

parameter_summary_map <- function(param_summary, au_param_summary, area){

  status_current <- as.symbol(colnames(param_summary)[grep("trend", colnames(param_summary)) - 1])

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

  assessment_units <- assessment_units %>% group_by(AU_ID, AU_Name) %>% dplyr::summarise()
  wql_streams_data <- sf::st_drop_geometry(wql_streams)
  wql_streams_shp <- wql_streams %>% group_by(STREAM_NAM, SEGMENT_ID) %>% dplyr::summarise()

# Create functions for mapping --------------------------------------------------------

  au_colors <- param_summary %>% group_by(AU_ID, Char_Name) %>%
    summarise(color = if_else(all(!!status_current %in% c("Unassessed", "Insufficient Data")),
                              "lightgray",
                              if_else(any(!!status_current == "Not Attaining"),
                                      "orange",
                                      "green")
                              )
              )

  param_summary <- param_summary %>% mutate(
    color = if_else(!!status_current %in% c("Unassessed", "Insufficient Data"),
                    "lightgray",
                    if_else(!!status_current == "Attaining",
                            "green",
                            "orange"))
  )
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

  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }

  # function to pull the selected parameter's status and trend and create a popup table for the station.
  # This function is called on the click of a station marker in the parameter summary map.
  popupTable <- function(station = NULL, AU = NULL, param){

    data <- param_summary %>% dplyr::rename(Parameter = Char_Name, Station_ID = MLocID, Station_Description = StationDes)

    if(!is.null(station)){
      data <- filter(data[, c(4, 3, 6:11)],
                     Station_ID == station, Parameter == param)
      # %>%
        # dplyr::select(-Parameter, -Station_ID)

      colnames(data) <- gsub("(?<=[0-9])[^0-9]", "-", colnames(data), perl = TRUE)
      colnames(data) <- gsub("_", " ", colnames(data), perl = TRUE)
      colnames(data) <- sapply(colnames(data), simpleCap, USE.NAMES = FALSE)

      table <- kable(data,
                     format = "html", row.names = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                      full_width = TRUE, font_size = 10)

    }
    if(!is.null(AU)){
      data <- dplyr::filter(data[, c(1, 4, 3, 6:11)],
                     AU_ID == AU, Parameter == param) %>%
        dplyr::select(-AU_ID)

      colnames(data) <- gsub("(?<=[0-9])[^0-9]", "-", colnames(data), perl = TRUE)
      colnames(data) <- gsub("_", " ", colnames(data), perl = TRUE)
      colnames(data) <- sapply(colnames(data), simpleCap, USE.NAMES = FALSE)

      table <- kable(data,
                     format = "html", row.names = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                      full_width = TRUE, font_size = 10)

    }
    return(table)
  }

  au_table <- function(AU = NULL, param){

    data <- au_param_summary %>% dplyr::rename(Parameter = Char_Name)

    data <- dplyr::filter(data, AU_ID == AU, Parameter == param)

    colnames(data) <- gsub("(?<=[0-9])[^0-9]", "-", colnames(data), perl = TRUE)
    colnames(data) <- gsub("_", " ", colnames(data), perl = TRUE)
    colnames(data) <- sapply(colnames(data), simpleCap, USE.NAMES = FALSE)

    table <- kable(data,
                   format = "html", row.names = FALSE) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                    full_width = TRUE, font_size = 10)

    return(table)
  }

  WQLpopupTable <- function(seg_ID = NULL, param = NULL){
    if(!is.null(seg_ID)){
      table <- kable(
        filter(wql_streams_data, SEGMENT_ID == seg_ID) %>%
          dplyr::select(Pollutant = Char_Name, Listing = LISTING_ST, Season = SEASON, TMDL = TMDL_INFO) %>% unique(),
                     format = "html", row.names = FALSE) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                      full_width = TRUE, font_size = 10)
    }
    return(table)
  }

# Create parameter summary map --------------------------------------------

  print("Creating Map...")

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
    addPolygons(data = area, fill = FALSE, group = "Assessment Area") %>%
    addMarkers(data = unique(param_summary[,c("AU_ID", "MLocID", "StationDes", "Lat_DD", "Long_DD")]),
               label = ~paste0(MLocID, ": ", StationDes),
               popup = ~paste0("<b>", MLocID, "</b>: ", StationDes, "<br>",
                               "AU: ", AU_ID),
               lat = ~Lat_DD,
               lng = ~Long_DD,
               group = "search"
    )

  if(nrow(wql_streams_data)>0){
    map <- map %>%
      addPolylines(data = wql_streams_shp,
                   opacity = 1,
                   weight = 3.5,
                   color = "red",
                   # popup = ~paste0("<b>", STREAM_NAM,
                   #                 "<br>Parameter:</b> ", Char_Name,
                   #                 "<br><b>Listing:</b> ", LISTING_ST),
                   popup = ~paste0("<b>", STREAM_NAM,
                                   # "<br>Parameter:</b> ", Char_Name,
                                   "<br></b><br>",
                                   sapply(SEGMENT_ID, WQLpopupTable, param = i, USE.NAMES = FALSE)),
                   popupOptions = popupOptions(maxWidth = 1200),
                   highlightOptions = highlightOptions(color = "red", weight = 8, opacity = 1),
                   label = ~STREAM_NAM,
                   smoothFactor = 1.5,
                   group = "WQ Listed Streams"
      )
  } else {print(paste("No water quality limited streams for", i))}

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
    psum <- param_summary %>% dplyr::filter(Char_Name == i)
    psum$z_offset <- if_else(!(psum[[status_current]] %in% c("Unassessed", "Insufficient Data") & psum$trend %in% c("Insufficient Data", "No Significant Trend")),
                             100, 0)
    psum_AU <- psum[!(psum[[status_current]] %in% c("Unassessed", "Insufficient Data") & psum$trend == "Insufficient Data"),]
    au_data <- dplyr::filter(assessment_units[, c("AU_ID", "AU_Name")], AU_ID %in% unique(psum_AU$AU_ID))
    au_data <- merge(au_data, dplyr::filter(au_colors, Char_Name == i)[,c("AU_ID", "color")], by = "AU_ID")
    # wql_streams_tmp <- dplyr::filter(wql_streams, Char_Name == i)

    if(nrow(au_data) > 0){
      au_data <- rmapshaper::ms_simplify(au_data)

      map <- map %>%
        addPolylines(data = au_data,
                     stroke = TRUE,
                     opacity = 0.8,
                     weight = 3,
                     color = ~color,
                     popup = ~paste0("<b>", AU_Name, "<br>",
                                     # "<br><b>Parameter:</b> ", i, "<br>",
                                     sapply(AU_ID, au_table, param = i, USE.NAMES = FALSE),
                                     sapply(AU_ID, popupTable, station = NULL, param = i, USE.NAMES = FALSE)
                     ),
                     popupOptions = popupOptions(maxWidth = 1200),
                     label = ~AU_ID,
                     smoothFactor = 2,
                     options = pathOptions(className = "assessmentUnits", interactive = TRUE),
                     highlightOptions = highlightOptions(color = "black", weight = 8, opacity = 1),
                     group = i
        )
    }

     map <- map %>%
      addAwesomeMarkers(data = psum,
                        lat = ~Lat_DD,
                        lng = ~Long_DD,
                        icon = awesomeIcons(icon = ~icon,
                                            iconColor = 'black',
                                            library = 'glyphicon',
                                            markerColor = ~color),
                        label = ~MLocID,
                        popup = ~paste0("<b>", StationDes, "<br>ID:</b> ", MLocID,
                                        "<br><b>AU ID:</b> ", AU_ID,
                                        "<br>",
                                        sapply(MLocID, popupTable, AU = NULL, param = i, USE.NAMES = FALSE)),
                        popupOptions = popupOptions(maxWidth = 1200),
                        labelOptions = list(className = "stationLabels", noHide = T, permanent = T, interactive = T,
                                            offset = c(-10,-25), opacity = 0.9, textsize = "14px", sticky = TRUE),
                        options = ~markerOptions(zIndexOffset = z_offset),
                        group = i
      )

  }

  map <- map %>%
    addLayersControl(baseGroups = sort(unique(param_summary$Char_Name)),
                     overlayGroups = c("Assessment Area", "WQ Listed Streams", "World Imagery", "Hydrography", "Land Cover (NLCD 2016)")) %>%
    hideGroup(c("World Imagery", "Hydrography", "Land Cover (NLCD 2016)", "WQ Listed Streams")) %>%
    addEasyButton(easyButton(
      icon = "fa-globe",
      title = "Zoom to assessment area",
      onClick = JS("function(btn, map){
                var groupLayer = map.layerManager.getLayerGroup('Assessment Area');
                map.fitBounds(groupLayer.getBounds());
                 }"))) %>%
    addEasyButton(easyButton(
      icon = "fa-sitemap",
      title = "Toggle Assessment Units",
      onClick = JS("function(btn, map){
    var elements = document.getElementsByClassName('assessmentUnits');
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
    )) %>%
    addEasyButton(easyButton(
      icon = "fa-map-marker",
      title = "Toggle Station Markers",
      onClick = JS("function(btn, map){
    var elements = document.getElementsByClassName('leaflet-pane leaflet-marker-pane');
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

  var shadows = document.getElementsByClassName('leaflet-pane leaflet-shadow-pane');
    var index;

    shadows = shadows.length ? shadows : [shadows];
  for (index = 0; index < shadows.length; index++) {
    shadow = shadows[index];

    if (isElementHidden(shadow)) {
      shadow.style.display = '';

      // If the shadow is still hidden after removing the inline display
      if (isElementHidden(shadow)) {
        shadow.style.display = 'block';
      }
    } else {
      shadow.style.display = 'none';
    }
  }
  function isElementHidden(shadow) {
    return window.getComputedStyle(shadow, null).getPropertyValue('display') === 'none';
  }
               }"
      )
    )) %>%
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
    )) %>%
    leaflet.extras::addSearchFeatures(targetGroups = "search",
                                      options = searchFeaturesOptions(openPopup = TRUE, textPlaceholder = "Search Station IDs...")) %>%
    htmlwidgets::onRender(jsCode = "function(el, x){
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
               }") %>% hideGroup("search")

  return(map)
}
