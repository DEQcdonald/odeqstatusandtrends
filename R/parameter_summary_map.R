#' Create parameter summary map for status and trends report
#'
#'
#' @param param_summary Parameter summary table from parameter_summary()
#' @param assess_summary Assessment summary table from parameter_summary()
#' @param area Shapefile for assessment areas
#' @param proj_dir Path to where 'Plot' folder is located.
#' @return map of status and trends results
#' @export
#' @examples parameter_summary_map(param_summary = parameter_summary_df)

parameter_summary_map <- function(param_summary, au_param_summary, area, proj_dir){

  setwd(proj_dir)

  load("//deqhq1/WQNPS/Status_and_Trend_Reports/2020-Revision/Oregon_target_data.RData")
  load(file = "//deqhq1/WQNPS/Status_and_Trend_Reports/Lookups_Statewide/huc_crosswalk.RData")

  state_target_data <- state_target_data %>% dplyr::filter(!is.na(target_value))
  status_current <- as.symbol(colnames(param_summary)[grep("trend", colnames(param_summary)) - 1])
  au_param_summary <- au_param_summary %>% dplyr::filter(AU_ID != "")

  print("Clipping summary table to shapefile...")
  param_shp <- sf::st_as_sf(param_summary, dim = "XY", coords = c("Long_DD", "Lat_DD"))
  area_sf <- sf::st_as_sf(area)
  param_shp <- sf::st_set_crs(param_shp, 4326)
  area_sf <- st_transform(area_sf, 4326)
  param_shp <- param_shp[lengths(sf::st_within(param_shp, area_sf)) == 1,]
  param_summary <- param_summary %>% dplyr::filter(MLocID %in% param_shp$MLocID)
  rm(list = c("param_shp", "area_sf"))

  lgnd <- base64enc::base64encode("//deqhq1/WQNPS/Status_and_Trend_Reports/Figures/map_legend.png")
  logo <- base64enc::base64encode("//deqhq1/WQNPS/Status_and_Trend_Reports/Figures/DEQ-logo-color-non-transp71x107.png")

  # Set up shapefiles for map -----------------------------------------------
  print("Processing shapefiles...")
  # query <- paste0("SELECT * FROM AssessmentUnits_OR_Dissolve WHERE AU_ID IN ('",
  #                 paste(unique(param_summary$AU_ID), collapse = "', '"), "')")

  assessment_units_lines <- sf::st_read(
    dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/WQ_Assessment_2018_20.gdb",
    layer = "AssessmentUnits_OR_Rivers_Coast",
    query = paste0("SELECT * FROM AssessmentUnits_OR_Rivers_Coast WHERE AU_ID IN ('",
                   paste(unique(param_summary$AU_ID), collapse = "', '"), "')"), stringsAsFactors = FALSE
  )
  assessment_units_ws <- sf::st_read(
    dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/WQ_Assessment_2018_20.gdb",
    layer = "AssessmentUnit_Watershed",
    query = paste0("SELECT * FROM AssessmentUnit_Watershed WHERE AU_ID IN ('",
                   paste(unique(param_summary$AU_ID), collapse = "', '"), "')"), stringsAsFactors = FALSE
  )
  assessment_units_bodies <- sf::st_read(
    dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/WQ_Assessment_2018_20.gdb",
    layer = "AssessmentUnit_Waterbodies",
    query = paste0("SELECT * FROM AssessmentUnits_OR_Waterbodies WHERE AU_ID IN ('",
                   paste(unique(param_summary$AU_ID), collapse = "', '"), "')"), stringsAsFactors = FALSE
  )

  if(unique(area$MAP) == "Columbia River"){

    columbia_aus <- sf::st_read(
      dsn = "//deqhq1/wqnps/Status_and_Trend_Reports/GIS/Assessment_Units.gdb",
      layer = "Columbia_River"
    )$AU_ID

    assessment_units_lines <- assessment_units_lines %>% dplyr::filter(AU_ID %in% columbia_aus)
    assessment_units_ws <- assessment_units_ws %>% dplyr::filter(AU_ID %in% columbia_aus)
    assessment_units_bodies <- assessment_units_bodies %>% dplyr::filter(AU_ID %in% columbia_aus)

  } else if(unique(area$MAP) == "Snake River"){

    snake_aus <- c(
      sf::st_read(
        dsn = "//deqhq1/wqnps/Status_and_Trend_Reports/GIS/Assessment_Units.gdb",
        layer = "Snake_River_Lines")$AU_ID,
      sf::st_read(
        dsn = "//deqhq1/wqnps/Status_and_Trend_Reports/GIS/Assessment_Units.gdb",
        layer = "Snake_River_waterbodies")$AU_ID,
      sf::st_read(
        dsn = "//deqhq1/wqnps/Status_and_Trend_Reports/GIS/Assessment_Units.gdb",
        layer = "Snake_River_Watershed")$AU_ID
    )

    assessment_units_lines <- assessment_units_lines %>% dplyr::filter(AU_ID %in% snake_aus)
    assessment_units_ws <- assessment_units_ws %>% dplyr::filter(AU_ID %in% snake_aus)
    assessment_units_bodies <- assessment_units_bodies %>% dplyr::filter(AU_ID %in% snake_aus)

  }

  agwqma <- sf::st_read(
    dsn = "//deqhq1/WQNPS/Status_and_Trend_Reports/GIS",
    layer = "ODA_AgWQMA",
    stringsAsFactors = FALSE
  )
  agwqma <- agwqma[,c("PlanName","AgWQ_Repor")]

  # wql_streams <- sf::st_read(
  #   dsn = "//deqhq1/WQNPS/Agriculture/Status_and_Trend_Analysis/R_support_files",
  #   layer = "WQL_Streams_2012",
  #   query = paste0("SELECT * FROM WQL_Streams_2012 WHERE HUC_4TH_CO IN ('",
  #                  paste(unique(param_summary$HUC8), collapse = "', '"), "')"),
  #   stringsAsFactors = FALSE
  # )
  
  huc_12s <- hucs[hucs$HUC_8 %in% unique(param_summary$HUC8),]$HUC_12
  
  if(unique(area$MAP) == "Columbia River"){
    
    wql_streams_lines <- sf::st_read(
      dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/WQ_Assessment_2018_20.gdb",
      layer = "Impaired_Pollutant_Rivers_Coast",
      query = paste0("SELECT * FROM Impaired_Pollutant_Rivers_Coast WHERE AU_ID IN ('",
                     paste(columbia_aus, collapse = "', '"), "')"),
      stringsAsFactors = FALSE
    )
    wql_streams_ws <- sf::st_read(
      dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/WQ_Assessment_2018_20.gdb",
      layer = "Impaired_Pollutant_Watershed",
      query = paste0("SELECT * FROM Impaired_Pollutant_Watershed WHERE AU_ID IN ('",
                     paste(columbia_aus, collapse = "', '"), "')"),
      stringsAsFactors = FALSE
    )
    wql_bodies <- sf::st_read(
      dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/WQ_Assessment_2018_20.gdb",
      layer = "Impaired_Pollutant_Waterbodies",
      query = paste0("SELECT * FROM Impaired_Pollutant_Waterbodies WHERE AU_ID IN ('",
                     paste(columbia_aus, collapse = "', '"), "')"),
      stringsAsFactors = FALSE
    )
    
  } else if(unique(area$MAP) == "Snake River"){

    wql_streams_lines <- sf::st_read(
      dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/WQ_Assessment_2018_20.gdb",
      layer = "Impaired_Pollutant_Rivers_Coast",
      query = paste0("SELECT * FROM Impaired_Pollutant_Rivers_Coast WHERE AU_ID IN ('",
                     paste(snake_aus, collapse = "', '"), "')"),
      stringsAsFactors = FALSE
    )
    wql_streams_ws <- sf::st_read(
      dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/WQ_Assessment_2018_20.gdb",
      layer = "Impaired_Pollutant_Watershed",
      query = paste0("SELECT * FROM Impaired_Pollutant_Watershed WHERE AU_ID IN ('",
                     paste(snake_aus, collapse = "', '"), "')"),
      stringsAsFactors = FALSE
    )
    wql_bodies <- sf::st_read(
      dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/WQ_Assessment_2018_20.gdb",
      layer = "Impaired_Pollutant_Waterbodies",
      query = paste0("SELECT * FROM Impaired_Pollutant_Waterbodies WHERE AU_ID IN ('",
                     paste(snake_aus, collapse = "', '"), "')"),
      stringsAsFactors = FALSE
    )
    
  } else {
  
  wql_streams_lines <- sf::st_read(
    dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/WQ_Assessment_2018_20.gdb",
    layer = "Impaired_Pollutant_Rivers_Coast",
    query = paste0("SELECT * FROM Impaired_Pollutant_Rivers_Coast WHERE HUC12 IN ('",
                   paste(huc_12s, collapse = "', '"), "')"),
    stringsAsFactors = FALSE
  )
  wql_streams_ws <- sf::st_read(
    dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/WQ_Assessment_2018_20.gdb",
    layer = "Impaired_Pollutant_Watershed",
    query = paste0("SELECT * FROM Impaired_Pollutant_Watershed WHERE HUC12 IN ('",
                   paste(huc_12s, collapse = "', '"), "')"),
    stringsAsFactors = FALSE
  )
  wql_bodies <- sf::st_read(
    dsn = "//deqhq1/GISLIBRARY/Base_Data/DEQ_Data/Water_Quality/WQ_2018_IntegratedReport/WQ_Assessment_2018_20.gdb",
    layer = "Impaired_Pollutant_Waterbodies",
    query = paste0("SELECT * FROM Impaired_Pollutant_Waterbodies WHERE HUC12 IN ('",
                   paste(huc_12s, collapse = "', '"), "')"),
    stringsAsFactors = FALSE
  )
  }

  if(NROW(wql_streams_lines) > 0){
    wql_streams_lines$Char_Name <- unlist(sapply(wql_streams_lines$Char_Name, AWQMS_Char_Names, USE.NAMES = FALSE))
  }
  if(NROW(wql_streams_ws) > 0){
    wql_streams_ws$Char_Name <- unlist(sapply(wql_streams_ws$Char_Name, AWQMS_Char_Names, USE.NAMES = FALSE))
  }
  if(NROW(wql_bodies) > 0){
    wql_bodies$Char_Name <- unlist(sapply(wql_bodies$Char_Name, AWQMS_Char_Names, USE.NAMES = FALSE))
  }

  assessment_units_lines <- sf::st_zm(assessment_units_lines, what = "ZM")
  assessment_units_ws <- sf::st_zm(assessment_units_ws, what = "ZM")
  assessment_units_bodies <- sf::st_zm(assessment_units_bodies, what = "ZM")
  wql_streams_lines <- sf::st_zm(wql_streams_lines, what = "ZM")
  wql_streams_ws <- sf::st_zm(wql_streams_ws, what = "ZM")
  wql_streams_bodies <- sf::st_zm(wql_bodies, what = "ZM")
  agwqma <- sf::st_zm(agwqma, what = "ZM")

  assessment_units_lines <- st_transform(assessment_units_lines, 4326)
  assessment_units_lines <- assessment_units_lines[,c("AU_ID", "AU_Name")] %>% dplyr::filter(AU_ID != "99")
  assessment_units_ws <- st_transform(assessment_units_ws, 4326)
  assessment_units_ws <- assessment_units_ws[,c("AU_ID", "AU_Name")] %>% dplyr::filter(AU_ID != "99")
  assessment_units_bodies <- st_transform(assessment_units_bodies, 4326)
  assessment_units_bodies <- assessment_units_bodies[,c("AU_ID", "AU_Name")] %>% dplyr::filter(AU_ID != "99")
  st_crs(wql_streams_lines)
  wql_streams_lines <- st_transform(wql_streams_lines, 4326)
  wql_streams_lines <- dplyr::filter(wql_streams_lines[, c("AU_Name", "AU_ID", "Period", "Char_Name", "IR_category")],
                               Char_Name %in% unique(param_summary$Char_Name))
  st_crs(wql_streams_ws)
  wql_streams_ws <- st_transform(wql_streams_ws, 4326)
  wql_streams_ws <- dplyr::filter(wql_streams_ws[, c("AU_Name", "AU_ID", "Period", "Char_Name", "IR_category")],
                               Char_Name %in% unique(param_summary$Char_Name))
  wql_bodies <- st_transform(wql_bodies, 4326)
  wql_bodies <- dplyr::filter(wql_bodies[, c("AU_Name", "AU_ID", "Period", "Char_Name", "IR_category")],
                                  Char_Name %in% unique(param_summary$Char_Name))
  # wql_streams <- wql_streams[lapply(wql_streams$`_ogr_geometry_`, length) != 0,]
  # wql_streams$TMDL_INFO <- vapply(strsplit(wql_streams$TMDL_INFO, "<a"), `[`, 1, FUN.VALUE=character(1))
  st_crs(agwqma)
  agwqma <- st_transform(agwqma, 4326)

  if(!any(class(area) == "sf")){
    area <- sf::st_as_sf(area)
  }

  st_crs(area)
  area <- st_transform(area, 4326)
  p_stns <- sf::st_as_sf(param_summary, coords = c("Long_DD", "Lat_DD"), crs = 4326)
  agwqma <- agwqma %>% dplyr::filter(lengths(st_intersects(., p_stns)) > 0)

  assessment_units_lines <- assessment_units_lines %>% dplyr::group_by(AU_ID, AU_Name) %>% dplyr::summarise()
  assessment_units_ws <- assessment_units_ws %>% dplyr::group_by(AU_ID, AU_Name) %>% dplyr::summarise()
  assessment_units_bodies <- assessment_units_bodies %>% dplyr::group_by(AU_ID, AU_Name) %>% dplyr::summarise()
  wql_streams_data <- bind_rows(sf::st_drop_geometry(wql_streams_lines),
                                sf::st_drop_geometry(wql_streams_ws),
                                sf::st_drop_geometry(wql_bodies))

  wql_streams_lines_shp <- wql_streams_lines %>% dplyr::group_by(AU_Name, AU_ID) %>% dplyr::summarise()
  wql_streams_ws_shp <- wql_streams_ws %>% dplyr::group_by(AU_Name, AU_ID) %>% dplyr::summarise()
  wql_bodies_shp <- wql_bodies %>% dplyr::group_by(AU_Name, AU_ID) %>% dplyr::summarise()

  # Create functions for mapping --------------------------------------------------------

  au_colors <- param_summary %>% dplyr::group_by(AU_ID, Char_Name, HUC8_Name, HUC8) %>%
    dplyr::summarise(color = dplyr::if_else(all(!!status_current %in% c("Unassessed", "Insufficient Data")),
                                            "lightgray",
                                            dplyr::if_else(any(!!status_current == "Not Attaining"),
                                                           "orange",
                                                           "green")
    )
    ) %>% dplyr::ungroup()

  param_summary <- param_summary %>% dplyr::mutate(
    color = dplyr::if_else(!!status_current %in% c("Unassessed", "Insufficient Data"),
                           "lightgray",
                           dplyr::if_else(!!status_current == "Attaining",
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
      data$Parameter <- odeqstatusandtrends::simpleCap(odeqstatusandtrends::AWQMS_to_standard(data$Parameter))

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
      data$Parameter <- odeqstatusandtrends::simpleCap(odeqstatusandtrends::AWQMS_to_standard(data$Parameter))

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
    data <- data %>% dplyr::select(-AU_Name, -HUC8_Name, -HUC8, -Stations, -Organizations)
    data <- data[, c("AU_ID", colnames(data)[colnames(data) != "AU_ID"])]
    data$Parameter <- odeqstatusandtrends::simpleCap(odeqstatusandtrends::AWQMS_to_standard(data$Parameter))

    colnames(data) <- gsub("(?<=[0-9])[^0-9]", "-", colnames(data), perl = TRUE)
    colnames(data) <- gsub("_", " ", colnames(data), perl = TRUE)
    colnames(data) <- sapply(colnames(data), simpleCap, USE.NAMES = FALSE)

    table <- knitr::kable(data, format = "html",
                          table.attr = "id=\"mytable\"", row.names = FALSE) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                                full_width = TRUE, font_size = 10)

    return(table)
  }

  target_table <- function(station = NULL, param){
    targets <- state_target_data %>%
      dplyr::filter(MLocID == station, Char_Name == param) %>%
      dplyr::mutate(Target = paste(target_value, target_units)) %>% 
      dplyr::select(Pollutant = Char_Name, Target, "Statistical Base" = target_stat_base,
                    "Applicable Period" = tmdl_period, TMDL = tmdl)
    if(any(!is.na(targets$Target))){
      targets <- targets %>% dplyr::filter(!is.na(Target))
      table <- knitr::kable(targets,
                                     format = "html", row.names = FALSE) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                                  full_width = TRUE, font_size = 10)
      return(table)
    } else {""
      # return(paste("No target assessed for", odeqstatusandtrends::AWQMS_to_standard(param), "at this station."))
    }
  }

  WQLpopupTable <- function(au_id = NULL, param = NULL){
    if(!is.null(au_id)){
      table <- knitr::kable(
        dplyr::filter(wql_streams_data, AU_ID == au_id) %>%
          dplyr::select(Pollutant = Char_Name, Listing = IR_category, Season = Period) %>% unique(),
        format = "html", row.names = FALSE) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                                  full_width = TRUE, font_size = 10)
    }
    return(table)
  }

  plot_html <- function(station, sub_name, param){
    if(param == "Dissolved oxygen (DO)"){
      paste(
        "DO plots<br>",
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_instantaneous.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_instantaneous.jpeg' style='width:600px' target='_blank'>Instantaneous</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_sdadmin.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_sdadmin.jpeg' style='width:600px' target='_blank'>7DADMin</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_30dadmean.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_30dadmean.jpeg' style='width:600px' target='_blank'>30DADMean</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_sdadmean.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_sdadmean.jpeg' style='width:600px' target='_blank'>7DADMean</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_minimum.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_minimum.jpeg' style='width:600px' target='_blank'>Minimum</a>")
        }
      )
    } else if(param %in% odeqstatusandtrends::AWQMS_Char_Names('bacteria')){
      paste(
        "Bacteria plots<br>",
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_ss.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_ss.jpeg' style='width:600px' target='_blank'>Single Sample</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_geomean.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_geomean.jpeg' style='width:600px' target='_blank'>Geomean</a>")
        }
      )
    } else if(param %in% odeqstatusandtrends::AWQMS_Char_Names('TSS')){
      paste(
        "TSS plots<br>",
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_single sample.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_single sample.jpeg' style='width:600px' target='_blank'>Single Sample</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_monthly mean.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_monthly mean.jpeg' style='width:600px' target='_blank'>Monthly Mean</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_annual mean.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_annual mean.jpeg' style='width:600px' target='_blank'>Annual Mean</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_no target.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_no target.jpeg' style='width:600px' target='_blank'>Single Sample</a>")
        }
      )
    } else if(param %in% odeqstatusandtrends::AWQMS_Char_Names('TP')){
      paste(
        "TP plots<br>",
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_single sample.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_single sample.jpeg' style='width:600px' target='_blank'>Single Sample</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_monthly median.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_monthly median.jpeg' style='width:600px' target='_blank'>Monthly Median</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_median over two consecutive years.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_median over two consecutive years.jpeg' style='width:600px' target='_blank'>Two Year Median</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_annual mean.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_annual mean.jpeg' style='width:600px' target='_blank'>Geomean</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_seasonal mean.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_seasonal mean.jpeg' style='width:600px' target='_blank'>Seasonal Mean</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_seasonal median.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_seasonal median.jpeg' style='width:600px' target='_blank'>Seasonal Median</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_no target.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_no target.jpeg' style='width:600px' target='_blank'>Single Sample</a>")
        }
      )
    } else if(param %in% odeqstatusandtrends::AWQMS_Char_Names('temperature')){
      paste(
        "Temperature plots<br>",
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, ".jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, ".jpeg' style='width:600px' target='_blank'>7DADM</a>")
        }
        ,
        if(file.exists(paste0("Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                              charnames[charnames$awqms == param, "file"], "_", station, "_daily maximum.jpeg"))){
          paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
                 charnames[charnames$awqms == param, "file"], "_", station, "_daily maximum.jpeg' style='width:600px' target='_blank'>Daily Maximum</a>")
        }
      )
    } else {
      # if(file.exists(paste0('Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
      #        charnames[charnames$awqms == param, "file"], "_", station, ".jpeg'))){
      paste0("<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
             charnames[charnames$awqms == param, "file"], "_", station, ".jpeg' style='width:600px' target='_blank'>Enlarge plot in new window</a>",
             "    ",
             "<a href='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
             charnames[charnames$awqms == param, "file"], "_", station, ".jpeg' download>Download plot</a>",
             "<br><br>",
             "<img src='Plots/", sub_name, "/", charnames[charnames$awqms == param, "folder"], "/",
             charnames[charnames$awqms == param, "file"], "_", station, ".jpeg' style='width:550px'>")
      # } else {paste0("No ", param, " data plotted for this station")}
    }
  }

  charnames <- data.frame(awqms = c("Temperature, water", "Dissolved oxygen (DO)", "pH", "Total suspended solids", odeqstatusandtrends::AWQMS_Char_Names('TP'),
                                    "Fecal Coliform", "Escherichia coli", "Enterococcus"),
                          folder = c("Temperature", "DO", "pH", "TSS", "TP", "Fecal_Coliform", "Ecoli", "Enterococcus"),
                          file = c("temp", "DO", "pH", "TSS", "TP", "FeColi", "Ecoli", "Enterococcus"),
                          stringsAsFactors = FALSE)

  # Create parameter summary map --------------------------------------------

  print("Creating Map...")

  map <- leaflet() %>% addTiles() %>%
    leaflet::addMapPane("Tiles", zIndex = 420) %>%
    leaflet::addMapPane("hydroTiles", zIndex = 425) %>%
    leaflet::addMapPane("assessment_area", zIndex = 430) %>%
    leaflet::addMapPane("agwqma", zIndex = 440) %>%
    leaflet::addMapPane("IRpolygons", zIndex = 450) %>%
    leaflet::addMapPane("IRpolylines", zIndex = 460) %>%
    leaflet::addMapPane("Status_polygons", zIndex = 505) %>%
    leaflet::addMapPane("Status_polylines", zIndex = 510) %>%
    leaflet::addMapPane("Status_points", zIndex = 515) %>% 
    # htmlwidgets::appendContent(HTML(table)) %>%
    #   htmlwidgets::onRender(
    #     "
    # function(el, x) {
    #   // our leaflet map is available as this
    #   mymap = this;
    # }
    # "
    #   ) %>%
    leaflet.esri::addEsriDependency() %>%
    leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery",
                              options = leaflet::tileOptions(pane = "Tiles")) %>%
    leaflet::addWMSTiles(baseUrl = 'https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2016_Land_Cover_L48/wms?',
                         group = "Land Cover (NLCD 2016)",
                         layers = "NLCD_2016_Land_Cover_L48",
                         options = leaflet::WMSTileOptions(version = '1.3.0',
                                                           format = 'image/png',
                                                           transparent = TRUE,
                                                           pane = "Tiles")) %>%
    leaflet::addWMSTiles("https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer",
                         group = "Hydrography",
                         options = leaflet::WMSTileOptions(format = "image/png",
                                                           transparent = TRUE,
                                                           pane = "hydroTiles"),
                         layers = "0")
  if(nrow(agwqma) > 0){
    map <- map %>%
      leaflet::addPolygons(data = agwqma, fill = TRUE, color = "blue", fillColor = "blue", opacity = 0.8, fillOpacity = 0.05, weight = 5,
                           group = "Ag WQ Management Areas", label = ~PlanName,
                           options = leaflet::pathOptions(pane = "agwqma"))
  }

  map <- map %>%
    leaflet::addPolygons(data = area, color = "black", fillOpacity = 0.1, group = "Assessment Area", 
                         opacity = 0.8, label = ~paste("Subbasin:", HU_8_NAME),
                         options = leaflet::leafletOptions(pane = "assessment_area")) %>%
    leaflet::addMarkers(data = unique(param_summary[,c("AU_ID", "MLocID", "StationDes", "Lat_DD", "Long_DD")]),
                        label = ~paste0(MLocID, ": ", StationDes),
                        popup = ~paste0("<b>", MLocID, "</b>: ", StationDes, "<br>",
                                        "AU: ", AU_ID),
                        lat = ~Lat_DD,
                        lng = ~Long_DD,
                        group = "search",
                        options = leaflet::pathOptions(pane = "Status_points")
    )

  if(nrow(wql_streams_ws_shp)>0){
    map <- map %>%
      leaflet::addPolygons(data = wql_streams_ws_shp,
                           opacity = 1,
                           weight = 3.5,
                           color = "#ff33be",
                           fillColor = "#ff33be",
                           fillOpacity = 0.25,
                           popup = ~paste0("<b>", AU_Name,
                                           # "<br>Parameter:</b> ", Char_Name,
                                           "<br></b><br>",
                                           sapply(AU_ID, WQLpopupTable, USE.NAMES = FALSE)),
                           popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 300),
                           highlightOptions = leaflet::highlightOptions(color = "black", weight = 8, opacity = 1),
                           label = ~AU_Name,
                           smoothFactor = 1.5,
                           group = "2018/2020 303(d)/305(b) IR Status",
                           options = leaflet::pathOptions(pane = "IRpolygons")
      )
  }

  if(nrow(wql_streams_ws_shp)>0){
    map <- map %>%
      leaflet::addPolylines(data = wql_streams_lines_shp,
                            opacity = 1,
                            weight = 3.5,
                            color = "#ff33be",
                            popup = ~paste0("<b>", AU_Name,
                                            # "<br>Parameter:</b> ", Char_Name,
                                            "<br></b><br>",
                                            sapply(AU_ID, WQLpopupTable, USE.NAMES = FALSE)),
                            popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 300),
                            highlightOptions = leaflet::highlightOptions(color = "black", weight = 8, opacity = 1),
                            label = ~AU_Name,
                            smoothFactor = 1.5,
                            group = "2018/2020 303(d)/305(b) IR Status",
                            options = leaflet::pathOptions(pane = "IRpolylines")
      )
  }

  if(nrow(wql_bodies_shp)>0){
    map <- map %>%
      leaflet::addPolylines(data = wql_bodies_shp,
                            opacity = 1,
                            weight = 3.5,
                            color = "#ff33be",
                            popup = ~paste0("<b>", AU_Name,
                                            # "<br>Parameter:</b> ", Char_Name,
                                            "<br></b><br>",
                                            sapply(AU_ID, WQLpopupTable, USE.NAMES = FALSE)),
                            popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 300),
                            highlightOptions = leaflet::highlightOptions(color = "black", weight = 8, opacity = 1),
                            label = ~AU_Name,
                            smoothFactor = 1.5,
                            group = "2018/2020 303(d)/305(b) IR Status",
                            options = leaflet::pathOptions(pane = "IRpolygons")
      )
  }

  # if(nrow(wql_streams_data) > 0){
  # map <- map %>%
  # leaflet::addPolylines(data = wql_streams,
  #              opacity = 0.7,
  #              weight = 2,
  #              color = "blue",
  #              # popup = ~paste0("<b>", STREAM_NAM,
  #              #                 "<br>Parameter:</b> ", Char_Name,
  #              #                 "<br><b>Listing:</b> ", LISTING_ST),
  #              popup = ~paste0("<b>", STREAM_NAM, "<br></b>",
  #                              sapply(SEGMENT_ID, WQLpopupTable, param = i, USE.NAMES = FALSE)),
  #              popupOptions = leaflet::popupOptions(maxWidth = 600),
  #              group = "WQL Streams"
  # )
  # } else {print(paste("No water quality limited streams for", i))}

  layer_groups <- NULL
  for(i in unique(param_summary$Char_Name)){
    print(paste("Adding layer for", i))
    standard_param <- odeqstatusandtrends::simpleCap(odeqstatusandtrends::AWQMS_to_standard(i))
    layer_groups <- c(layer_groups, standard_param)
    psum <- param_summary %>% dplyr::filter(Char_Name == i)
    psum$z_offset <- dplyr::if_else(!(psum[[status_current]] %in% c("Unassessed", "Insufficient Data") & psum$trend %in% c("Insufficient Data", "No Significant Trend")),
                                    100, 0)
    psum_AU <- psum[!(psum[[status_current]] %in% c("Unassessed", "Insufficient Data")),]
    au_data <- dplyr::filter(assessment_units_lines[, c("AU_ID", "AU_Name")], AU_ID %in% unique(psum_AU$AU_ID))
    au_data <- merge(au_data, dplyr::filter(au_colors, Char_Name == i)[,c("AU_ID", "color", "HUC8_Name", "HUC8")], by = "AU_ID")
    au_data_ws <- dplyr::filter(assessment_units_ws[, c("AU_ID", "AU_Name")], AU_ID %in% unique(psum_AU$AU_ID))
    au_data_ws <- merge(au_data_ws, dplyr::filter(au_colors, Char_Name == i)[,c("AU_ID", "color", "HUC8_Name", "HUC8")], by = "AU_ID")
    au_data_bodies <- dplyr::filter(assessment_units_bodies[, c("AU_ID", "AU_Name")], AU_ID %in% unique(psum_AU$AU_ID))
    au_data_bodies <- merge(au_data_bodies, dplyr::filter(au_colors, Char_Name == i)[,c("AU_ID", "color", "HUC8_Name", "HUC8")], by = "AU_ID")

    # au_data <- au_colors %>% dplyr::filter(Char_Name == i)
    # wql_streams_tmp <- dplyr::filter(wql_streams, Char_Name == i)
    # green_ids <- au_data[au_data$color == "green", ]$AU_ID

    if(nrow(au_data) > 0){
      au_data <- rmapshaper::ms_simplify(au_data)

      map <- map %>%
        leaflet::addPolylines(data = au_data,
                              stroke = TRUE,
                              opacity = 0.8,
                              weight = 3,
                              color = ~color,
                              popup = ~paste0("<b>", AU_Name, "<br>",
                                              "<b>HUC8: </b>", HUC8_Name, " (",
                                              HUC8, ")<br>",
                                              # "<br><b>Parameter:</b> ", i, "<br>",
                                              sapply(AU_ID, au_table, param = i, USE.NAMES = FALSE),
                                              sapply(AU_ID, popupTable, station = NULL, param = i, USE.NAMES = FALSE)
                              ),
                              popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 300),
                              label = ~AU_Name,
                              smoothFactor = 2,
                              options = leaflet::pathOptions(className = "assessmentUnits", interactive = TRUE,
                                                             pane = "Status_polylines"),
                              highlightOptions = leaflet::highlightOptions(color = "black", weight = 8, opacity = 1),
                              group = standard_param
        )
    }
    if(nrow(au_data_bodies) > 0){
      map <- map %>%
        leaflet::addPolygons(data = au_data_bodies,
                             stroke = TRUE,
                             opacity = 0.9,
                             weight = 3,
                             color = ~color,
                             fillOpacity = 0.1,
                             fillColor = ~color,
                             popup = ~paste0("<b>", AU_Name, "<br>",
                                             "<b>HUC8: </b>", HUC8_Name, " (",
                                             HUC8, ")<br>",
                                             # "<br><b>Parameter:</b> ", i, "<br>",
                                             sapply(AU_ID, au_table, param = i, USE.NAMES = FALSE),
                                             sapply(AU_ID, popupTable, station = NULL, param = i, USE.NAMES = FALSE)
                             ),
                             popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 300),
                             label = ~AU_Name,
                             smoothFactor = 2,
                             options = leaflet::pathOptions(className = "assessmentUnits", interactive = TRUE,
                                                            pane = "Status_polygons"),
                             highlightOptions = leaflet::highlightOptions(color = "black", weight = 8, opacity = 1),
                             group = standard_param
        )
    }
    if(nrow(au_data_ws) > 0){
      map <- map %>%
        leaflet::addPolygons(data = au_data_ws,
                             stroke = TRUE,
                             opacity = 0.9,
                             weight = 3,
                             color = ~color,
                             fillOpacity = 0.1,
                             fillColor = ~color,
                             popup = ~paste0("<b>", AU_Name, "<br>",
                                             "<b>HUC8: </b>", HUC8_Name, " (",
                                             HUC8, ")<br>",
                                             # "<br><b>Parameter:</b> ", i, "<br>",
                                             sapply(AU_ID, au_table, param = i, USE.NAMES = FALSE),
                                             sapply(AU_ID, popupTable, station = NULL, param = i, USE.NAMES = FALSE)
                             ),
                             popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 300),
                             label = ~AU_Name,
                             smoothFactor = 2,
                             options = leaflet::pathOptions(className = "assessmentUnits", interactive = TRUE,
                                                            pane = "Status_polygons"),
                             highlightOptions = leaflet::highlightOptions(color = "black", weight = 8, opacity = 1),
                             group = standard_param
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
    #                               highlightOptions = leaflet::highlightOptions(color = "black", weight = 8, opacity = 1, bringToFront = TRUE),
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
      leaflet::addAwesomeMarkers(data = psum,
                                 lat = ~Lat_DD,
                                 lng = ~Long_DD,
                                 icon = leaflet::awesomeIcons(icon = ~icon,
                                                              iconColor = 'black',
                                                              library = 'glyphicon',
                                                              markerColor = ~color),
                                 label = ~MLocID,
                                 popup = ~paste0("<b>", StationDes, "<br>ID:</b> ", MLocID,
                                                 "<br><b>AU ID:</b> ", AU_ID,
                                                 "<br>",
                                                 sapply(MLocID, target_table, param = i, USE.NAMES = FALSE),
                                                 sapply(MLocID, popupTable, AU = NULL, param = i, USE.NAMES = FALSE),
                                                 mapply(plot_html, station = MLocID, sub_name = HUC8_Name, param = i, USE.NAMES = FALSE)
                                 ),
                                 popupOptions = leaflet::popupOptions(maxWidth = 600, maxHeight = 300),
                                 labelOptions = list(className = "stationLabels", noHide = T, permanent = T, interactive = T,
                                                     offset = c(-10,-25), opacity = 0.9, textsize = "14px", sticky = TRUE),
                                 options = ~leaflet::markerOptions(zIndexOffset = z_offset, riseOnHover = TRUE,
                                                                   pane = "Status_points"),
                                 group = standard_param
      )

  }

  map <- map %>%
    leaflet::addEasyButton(leaflet::easyButton(
      position = "topright",
      icon = "fa-align-justify",
      title = "Toggle Layers Control",
      id = 'layerToggle',
      onClick = JS("function(btn, map){
    var elements = document.getElementsByClassName('leaflet-control-layers leaflet-control-layers-expanded leaflet-control');
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
    leaflet::addLayersControl(baseGroups = sort(layer_groups),
                              overlayGroups = c("2018/2020 303(d)/305(b) IR Status", "Ag WQ Management Areas", "Assessment Area", 
                                                "Hydrography", "Land Cover (NLCD 2016)", "World Imagery"),
                              options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
    leaflet::hideGroup(c("World Imagery", "Hydrography", "Ag WQ Management Areas", "Land Cover (NLCD 2016)", "2018/2020 303(d)/305(b) IR Status")) %>%
    leaflet::addControl(position = "bottomleft", className = "legend",
                        html = sprintf('<html><body><div style="opacity:0.95">
                                        <img width="375" height="180" src="data:image/png;base64,%s">
                            </div></body></html>', lgnd)) %>%
    leaflet::addControl(position = "bottomright", className = "logo",
                        html = sprintf('<html><body><div style="opacity:1">
               <a href="https://www.oregon.gov/deq/wq/programs/Pages/wqstatustrends.aspx">
                                        <img width="60" src="data:image/png;base64,%s">
                            </a></div></body></html>', logo)) %>%
    leaflet::addEasyButton(leaflet::easyButton(
      icon = "fa-globe",
      title = "Zoom to assessment area",
      onClick = JS("function(btn, map){
                var groupLayer = map.layerManager.getLayerGroup('Assessment Area');
                map.fitBounds(groupLayer.getBounds());
                 }"))) %>%
    leaflet::addEasyButton(leaflet::easyButton(
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
    leaflet::addEasyButton(leaflet::easyButton(
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
    leaflet::addEasyButton(leaflet::easyButton(
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
    leaflet::addEasyButton(leaflet::easyButton(
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
                                    options = leaflet.extras::searchFeaturesOptions(openPopup = TRUE, textPlaceholder = "Search Station IDs...")) %>%
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

  var layerToggle = document.getElementsByClassName('leaflet-bar easy-button-container leaflet-control')[4];
  layerToggle.style.float = 'none';
  layerToggle.style.backgroundColor = 'white';
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
  leaflet::hideGroup("search") %>%
    htmlwidgets::appendContent(tags$head(tags$meta(name="viewport", content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no")))

  return(map)
}
