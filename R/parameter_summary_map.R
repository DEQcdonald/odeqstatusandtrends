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
  au_param_summary <- au_param_summary %>% dplyr::filter(AU_ID != "")

  print("Clipping summary table to shapefile...")
  param_shp <- st_as_sf(param_summary, dim = "XY", coords = c("Long_DD", "Lat_DD"))
  area_sf <- st_as_sf(area)
  param_shp <- st_set_crs(param_shp, 4326)
  area_sf <- st_transform(area_sf, 4326)
  param_shp <- param_shp[lengths(st_within(param_shp, area_sf)) == 1,]
  param_summary <- param_summary %>% dplyr::filter(MLocID %in% param_shp$MLocID)
  rm(list = c("param_shp", "area_sf"))

  lgnd <- base64enc::base64encode("//deqhq1/WQNPS/Status_and_Trend_Reports/Figures/map_legend.png")

# Set up shapefiles for map -----------------------------------------------
  print("Processing shapefiles...")
  query <- paste0("SELECT * FROM AssessmentUnits_OR_Dissolve WHERE AU_ID IN ('",
                  paste(unique(param_summary$AU_ID), collapse = "', '"), "')")

  assessment_units <- sf::st_read(
    dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/Assessment_2018_2020.gdb",
    layer = "AssessmentUnits_OR_Dissolve",
    query = query, stringsAsFactors = FALSE
  )

  agwqma <- sf::st_read(
    dsn = "//deqhq1/WQNPS/Status_and_Trend_Reports/GIS",
    layer = "ODA_AgWQMA",
    stringsAsFactors = FALSE
  )
  agwqma <- agwqma[,c("PlanName","AgWQ_Repor")]

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
  agwqma <- sf::st_zm(agwqma, what = "ZM")

  st_crs(assessment_units)
  assessment_units <- st_transform(assessment_units, 4326)
  assessment_units <- assessment_units[,c("AU_ID", "AU_Name")] %>% dplyr::filter(AU_ID != "99")
  st_crs(wql_streams)
  wql_streams <- st_transform(wql_streams, 4326)
  wql_streams <- dplyr::filter(wql_streams[, c("STREAM_NAM", "SEGMENT_ID", "SEASON", "Char_Name", "LISTING_ST", "TMDL_INFO")], Char_Name %in% unique(param_summary$Char_Name))
  wql_streams <- wql_streams[lapply(wql_streams$`_ogr_geometry_`, length) != 0,]
  wql_streams$TMDL_INFO <- vapply(strsplit(wql_streams$TMDL_INFO, "<a"), `[`, 1, FUN.VALUE=character(1))
  st_crs(agwqma)
  agwqma <- st_transform(agwqma, 4326)

  if(!any(class(area) == "sf")){
    area <- sf::st_as_sf(area)
  }

  st_crs(area)
  area <- st_transform(area, 4326)
  p_stns <- st_as_sf(param_summary, coords = c("Long_DD", "Lat_DD"), crs = 4326)
  agwqma <- agwqma %>% dplyr::filter(lengths(st_intersects(., p_stns)) > 0)

  assessment_units <- assessment_units %>% group_by(AU_ID, AU_Name) %>% dplyr::summarise()
  wql_streams_data <- sf::st_drop_geometry(wql_streams)
  wql_streams_shp <- wql_streams %>% group_by(STREAM_NAM, SEGMENT_ID) %>% dplyr::summarise()

# Create functions for mapping --------------------------------------------------------

  au_colors <- param_summary %>% group_by(AU_ID, Char_Name) %>%
    dplyr::summarise(color = if_else(all(!!status_current %in% c("Unassessed", "Insufficient Data")),
                              "lightgray",
                              if_else(any(!!status_current == "Not Attaining"),
                                      "orange",
                                      "green")
                              )
              ) %>% ungroup()

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

  # function to pull the selected parameter's status and trend and create a popup table for the station.
  # This function is called on the click of a station marker in the parameter summary map.
  popupTable <- function(station = NULL, AU = NULL, param){

    data <- param_summary %>% dplyr::rename(Parameter = Char_Name, Station_ID = MLocID, Station_Description = StationDes)

    if(!is.null(station)){
      data <- dplyr::filter(data[, c(2, 1, grep("status|trend", colnames(param_summary)))],
                     Station_ID == station, Parameter == param)
      # %>%
        # dplyr::select(-Parameter, -Station_ID)

      colnames(data) <- gsub("(?<=[0-9])[^0-9]", "-", colnames(data), perl = TRUE)
      colnames(data) <- gsub("_", " ", colnames(data), perl = TRUE)
      colnames(data) <- sapply(colnames(data), simpleCap, USE.NAMES = FALSE)

      table <- knitr::kable(data,
                            format = "html", row.names = FALSE) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                             full_width = TRUE, font_size = 10)

    }
    if(!is.null(AU)){
      data <- dplyr::filter(data[, c(2, 1, 3, grep("status|trend", colnames(param_summary)))],
                            AU_ID == AU, Parameter == param) %>%
        dplyr::select(-AU_ID)

      colnames(data) <- gsub("(?<=[0-9])[^0-9]", "-", colnames(data), perl = TRUE)
      colnames(data) <- gsub("_", " ", colnames(data), perl = TRUE)
      colnames(data) <- sapply(colnames(data), simpleCap, USE.NAMES = FALSE)

      table <- knitr::kable(data,
                     format = "html", row.names = FALSE) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
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

    table <- knitr::kable(data, format = "html",
                   table.attr = "id=\"mytable\"", row.names = FALSE) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                    full_width = TRUE, font_size = 10)

    return(table)
  }

  WQLpopupTable <- function(seg_ID = NULL, param = NULL){
    if(!is.null(seg_ID)){
      table <- knitr::kable(
        dplyr::filter(wql_streams_data, SEGMENT_ID == seg_ID) %>%
          dplyr::select(Pollutant = Char_Name, Listing = LISTING_ST, Season = SEASON, TMDL = TMDL_INFO) %>% unique(),
                     format = "html", row.names = FALSE) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                      full_width = TRUE, font_size = 10)
    }
    return(table)
  }

  plot_html <- function(station, sub_name, param){
    if(param == "Dissolved oxygen (DO)"){
      paste(
        "DO plots (link will fail if plot is unavailable)<br>",
        # if(file.exists(paste0('Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
        #        charnames[charnames$awqms == param, "file"], "_", station, "_instantaneous.jpeg'))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_instantaneous.jpeg' style='width:600px' target='_blank'>Instantaneous</a>")
        # }
    ,
        # if(file.exists(paste0('Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
        #        charnames[charnames$awqms == param, "file"], "_", station, "_sdadmin.jpeg'))){
        paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
               charnames[charnames$awqms == param, "file"], "_", station, "_sdadmin.jpeg' style='width:600px' target='_blank'>7DADMin</a>")
        # }
    ,
        # if(file.exists(paste0('Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
        #        charnames[charnames$awqms == param, "file"], "_", station, "_30dadmean.jpeg'))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_30dadmean.jpeg' style='width:600px' target='_blank'>30DADMean</a>")
        # }
    ,
        # if(file.exists(paste0('Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
        #        charnames[charnames$awqms == param, "file"], "_", station, "_sdadmean.jpeg'))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_sdadmean.jpeg' style='width:600px' target='_blank'>7DADMean</a>")
        # }
    ,
        # if(file.exists(paste0('Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
        #        charnames[charnames$awqms == param, "file"], "_", station, "_minimum.jpeg'))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_minimum.jpeg' style='width:600px' target='_blank'>Minimum</a>")
        # }
      )
    } else {
      # if(file.exists(paste0('Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
      #        charnames[charnames$awqms == param, "file"], "_", station, ".jpeg'))){
        paste0("<img src='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
               charnames[charnames$awqms == param, "file"], "_", station, ".jpeg' style='width:600px'>")
      # } else {paste0("No ", param, " data plotted for this station")}
    }
  }

  charnames <- data.frame(awqms = c("Temperature, water", "Dissolved oxygen (DO)", "pH", "Total suspended solids", "Phosphate-phosphorus",
                                       "Fecal Coliform", "Escherichia coli", "Enterococcus"),
                             folder = c("Temperature", "DO", "pH", "TSS", "TP", "Fecal Coliform", "Escherichia coli", "Enterococcus"),
                             file = c("temp", "DO", "pH", "TSS", "TP", "Fecal Coliform", "Escherichia coli", "Enterococcus"),
                          stringsAsFactors = FALSE)

# Create parameter summary map --------------------------------------------

  print("Creating Map...")

  map <- leaflet() %>% addTiles() %>%
    # htmlwidgets::appendContent(HTML(table)) %>%
  #   htmlwidgets::onRender(
  #     "
  # function(el, x) {
  #   // our leaflet map is available as this
  #   mymap = this;
  # }
  # "
  #   ) %>%
    addEsriDependency() %>%
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
                layers = "0")
  if(nrow(agwqma) > 0){
    map <- map %>%
      addPolygons(data = agwqma, fill = TRUE, color = "black", fillColor = "black", opacity = 0.8, weight = 5,
                  group = "Ag WQ Management Areas", label = ~PlanName)
    }

  map <- map %>%
    addPolygons(data = area, fill = FALSE, group = "Assessment Area", opacity = 0.8, label = "Assessment Area") %>%
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

    # au_data <- au_colors %>% dplyr::filter(Char_Name == i)
    # wql_streams_tmp <- dplyr::filter(wql_streams, Char_Name == i)
    # green_ids <- au_data[au_data$color == "green", ]$AU_ID

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
    #   for(j in c("lightgray", "green", "orange")){
    #     au_ids <- au_data %>% dplyr::filter(color == j)
    #     if(nrow(au_ids)>0){
    #       for(k in au_ids$AU_ID){
    #         map <- map %>%
    #           addEsriFeatureLayer(url = "https://deq14.deq.state.or.us/arcgis/rest/services/WQ/WQ_2018_IR_V3/MapServer/14",
    #                               options = featureLayerOptions(
    #                                 where = paste0("AU_ID IN ('", paste(k, collapse = "', '"), "')")
    #                                 ),
    #                               highlightOptions = highlightOptions(color = "black", weight = 8, opacity = 1, bringToFront = TRUE),
    #                               color = j, fill = FALSE, group = i, opacity = 1, labelProperty = "AU_Name",
    #                               pathOptions = leaflet::pathOptions(className = "assessmentUnits", interactive = TRUE)
    #                               ,
    #                               popupProperty = JS(paste0(
    #                                 "function(feature) {",
    #                                 "var input, filter, table, tr, td, i, txtValue;",
    #                                 "input = {AU_ID};",
    #                                 "table = ", table, ";",
    #                                 "tr = table.getElementsByTagName('tr');",
    #                                 "for (i = 0; i < tr.length; i++) {
    #                                 td = tr[i].getElementsByTagName('td')[0];
    #                                 if (td) {
    #                                 txtValue = td.textContent || td.innerText;
    #                                 if (txtValue.toUpperCase().indexOf(filter) > -1) {
    #                                 tr[i].style.display = '';
    #                                 } else {
    #                                 tr[i].style.display = 'none';
    #                                 }
    #                                 }
    #                                 }",
    #                                 "  return L.Util.template(",
    #                                 " '",
    #                                 "<b>{AU_Name}<br>",
    #                                 table,
    #                                 # gsub("\\n", "",
    #                                 #      sapply(k, au_table, param = i, USE.NAMES = FALSE)),
    #                                 # gsub("\\n", "",
    #                                 #      sapply(k, popupTable, station = NULL, param = i, USE.NAMES = FALSE)),
    #                                 "   ' ,",
    #                                 "    feature.properties",
    #                                 "  );",
    #                                 "}")
    #                               ),
    #                               popupOptions = leaflet::popupOptions(maxWidth = 1200),
    #           )
    #       }
    #     }
    #   }
    # }

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
                                        sapply(MLocID, popupTable, AU = NULL, param = i, USE.NAMES = FALSE),
                                        mapply(plot_html, station = MLocID, sub_name = HUC8_Name, param = i, USE.NAMES = FALSE)
                                        ),
                        popupOptions = popupOptions(maxWidth = 1200),
                        labelOptions = list(className = "stationLabels", noHide = T, permanent = T, interactive = T,
                                            offset = c(-10,-25), opacity = 0.9, textsize = "14px", sticky = TRUE),
                        options = ~markerOptions(zIndexOffset = z_offset, riseOnHover = TRUE),
                        group = i
      )

  }

  map <- map %>%
    addLayersControl(baseGroups = sort(unique(param_summary$Char_Name)),
                     overlayGroups = c("Assessment Area", "WQ Listed Streams", "Ag WQ Management Areas",
                                       "World Imagery", "Hydrography", "Land Cover (NLCD 2016)")) %>%
    hideGroup(c("World Imagery", "Hydrography", "Ag WQ Management Areas", "Land Cover (NLCD 2016)", "WQ Listed Streams")) %>%
    addControl(position = "bottomleft", className = "legend",
               html = sprintf('<html><body><div style="opacity:0.8">
                                        <img width="350" height="175" src="data:image/png;base64,%s">
                            </div></body></html>', lgnd)) %>%
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
    addEasyButton(easyButton(
      position = "bottomleft",
      icon = "fa-info-circle",
      title = "Toggle Legend",
      onClick = JS("function(btn, map){
    var elements = document.getElementsByClassName('legend');
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
    # registerPlugin(htmlDependency(name = "leaflet-easyprint",
    #                               version = "2.2.1",
    #                               src = "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js",
    #                               script = "bundle.js")) %>%
    # onRender(jsCode = "function(el, x) {
    #           L.easyPrint({
    #               title: 'Download Map (CDL layer not available for download)',
    #               position: 'topleft',
    #               sizeModes: ['Current'],
    #               exportOnly: true,
    #               filename: 'map',
    #           }).addTo(this);}") %>%
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
               }") %>%
    # htmlwidgets::onRender(paste0(
    #   "function(el, x) {",
    #   "var assessmentUnits = L.esri.featureLayer({",
    #   "url: 'https://deq14.deq.state.or.us/arcgis/rest/services/WQ/WQ_2018_IR_V3/MapServer/14',",
    #   "where: ",
    #   '"', "AU_ID IN ('", paste0("AU_ID IN ('", paste(k, collapse = "', '"), "')"), '",',
    #   # "OR_LK_1709001203_02_100869','OR_SR_1709000110_02_104584','OR_SR_1709000204_02_103787','OR_SR_1709000301_02_103796')", '",',
    #   "style: function (feature) {",
    #   "var c;",
    #   "switch (feature.properties.AU_ID) {",
    #   'case "', paste0(au_data[au_data$color == "green", ]$AU_ID, collapse = ","), '"',
    #   "c = '#179639';",
    #   "break;",
    #   'case "', paste0(au_data[au_data$color == "orange", ]$AU_ID, collapse = ","), '"',
    #   "c = '#fc923a';",
    #   "break;",
    #   'case "', paste0(au_data[au_data$color == "lightgray", ]$AU_ID, collapse = ","), '"',
    #   "c = '#a3a3a3';",
    #   "break;",
    #   "default: '#ff00f2';",
    #   "}",
    #   "return {color: c, opacity: 1, weight: 8};",
    #   "}",
    #   "}).addTo(this);",
    #   "assessmentUnits.bindPopup(",
    #   "function (layer) {",
    #   "var input, table, tr, td, i, txtValue;",
    #   "input = layer.feature.properties.AU_ID;",
    #   "table = document.getElementById('mytable');",
    #   "tr = table.getElementsByTagName('tr');",
    #   "for (i = 0; i < tr.length; i++) { ",
    #   "td = tr[i].getElementsByTagName('td')[0];",
    #   "if (td) {",
    #   "txtValue = td.innerText || td.innerHTML;",
    #   "if (txtValue.toUpperCase().indexOf(input) > -1) {",
    #   "tr[i].style.display = '';",
    #   "} else {",
    #   "tr[i].style.display = 'none';",
    #   "}}}",
    #   ";",
    #   "return table",
    #   "}, {maxWidth : 1200});",
    #   "}"
    # )
    # ) %>%
    hideGroup("search")

  return(map)
}
