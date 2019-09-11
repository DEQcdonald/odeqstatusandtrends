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
                                            "Phosphate-phosphorus", "Phosphate-phosphorus", "Phosphate-phosphorus",
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
