#' AWQMS Characteristic Names
#'
#' Converts input parameters to AWQMS characteristics names
#' @param parameters A list of parameters to convert.
#' @return A list of relevant AWQMS parameters
#' @export
#' @examples
#' AWQMS_Char_Names(parameters = c("Temperature", "Bacteria", "TSS"))

AWQMS_Char_Names <- function(parameters){

  if(is.null(parameters)){return(NULL)}
  parameters <- tolower(parameters)

  #### Expand bacteria to include fecal and enterococcus ####
  if(any(parameters %in% c('Bacteria', 'bacteria'))) {
    parameters <- c(parameters, c('Ecoli','Fecal coliform','Enterococcus'))
    parameters <- unique(parameters[!parameters %in% c('Bacteria', 'bacteria')])
  }

  #### Lookup table for parameters and their AWQMS database name ####
  parms.lookup <- data.frame(General = c("tss",
                                         "temperature",
                                         "tp", "total phosphorus", "phosphorus",
                                         "ph",
                                         "fecal coliform",
                                         "ecoli", "e. coli", "e.coli",
                                         "enterococcus",
                                         "do",
                                         "dissolved oxygen"),
                             AWQMS.Name = c("Total suspended solids",
                                            "Temperature, water",
                                            "Total Phosphorus, mixed forms", "Total Phosphorus, mixed forms", "Total Phosphorus, mixed forms",
                                            "pH",
                                            "Fecal Coliform",
                                            "Escherichia coli", "Escherichia coli", "Escherichia coli",
                                            "Enterococcus",
                                            "Dissolved oxygen (DO)",
                                            "Dissolved oxygen (DO)"),
                             stringsAsFactors = FALSE
  )

  #### Convert parameters to AWQMS parameter names for query ####
  AWQMS.parms <- parms.lookup[parms.lookup$General %in% tolower(parameters),]$AWQMS.Name

  if(identical(AWQMS.parms, character(0))){
    AWQMS.parms <- parameters
  }

  return(AWQMS.parms)
}

#' Convert AWQMS Characteristic Names to standard language
#'
#' Converts AWQMS Characteristic Names to standard language
#' @param AWQMS_params A list of parameters to convert.
#' @return A list of relevant parameters converted to standard language
#' @export
#' @examples
#' AWQMS_to_standard(parameters = c("Temperature, water", "Dissolved oxygen (DO)", "Phosphate-phosphorus"))

AWQMS_to_standard <- function(AWQMS_params){

  if(is.null(AWQMS_params)){return(NULL)}
  standard_parms <- tolower(AWQMS_params)

  #### Capitalize Escherichia coli ####
  standard_parms <- gsub("escherichia coli", "Escherichia coli", standard_parms)
  standard_parms <- gsub("\\bph\\b", "pH", standard_parms)
  standard_parms <- gsub("dissolved oxygen \\(do\\)", "dissolved oxygen", standard_parms)
  standard_parms <- gsub("Total Phosphorus, mixed forms", "total phosphorus", standard_parms)
  standard_parms <- gsub("temperature, water", "temperature", standard_parms)

  return(standard_parms)
}

#' Capitalize the first letter of every word in a string
#'
#' Takes a given string and capitalizes the first letter of every word
#' @param string A character string to capitalize
#' @return A capitalized string
#' @export
#' @examples
#' simpleCap(string = "total suspended solids")

simpleCap <- function(string) {
  if(string != "pH"){
    s <- strsplit(string, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  } else {paste(string)}
}
