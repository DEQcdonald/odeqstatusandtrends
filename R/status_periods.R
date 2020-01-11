#' Define status assessment periods
#'
#' Divides the entire assessment period as defined by year_range into shorter intervals based on the number of periods and
#' codes the datetime values according to which interval they belong to.
#'
#'
#' @param datetime Vector of datetimes in POSIXct format.
#' @param periods Number of shorter intervals to divide the total assesment period into. Default is 4.
#' @param year_range Vector of the minimum and maximum years that define the total assessment period. If NULL, the year range provided in datetime is used.
#' @param bins_only If TRUE, will return a vector of the each unique character values that define the calendar year range for each assessment period. Default is FALSE.
#' @return Vector of character values that define the calendar year range for each assessment period
#' @export
#' @examples
#' status_periods(datetime = data_assessed$sample_datetime, periods=4, year_range = c(start_year:end_year))
#'
status_periods <- function(datetime=NULL, periods=4, year_range=NULL, bins_only=FALSE) {

  #datetime <- data_assessed$sample_datetime
  #year_range <- c(1998,2018)
  #periods <- 4

  if(!bins_only) {
    
    if(is.null(datetime)) {
      stop("datetime is NULL")
    }
    
    if(!lubridate::is.POSIXct(datetime)) {
      stop("datetime not in POSIXct")
    }
    
    data_years <- lubridate::year(datetime)
  }

  if(is.null(periods)) {
    stop("periods is NULL")
  }
  
  if(is.null(year_range)){
    year_range <- c(min(data_years, na.rm = TRUE), max(data_years, na.rm = TRUE))
  }

  years <- year_range[2]:year_range[1]
  breaks <- seq(year_range[2], year_range[1], by =(-1*periods))
  cols <- sapply(breaks, function(x){
    start <- x - periods + 1
    return(paste0(
      start, "_", x))
  })

  if(bins_only) {
    return(cols)
  }

  bins <- lapply(breaks, function(x){
    start <- x - periods + 1
    return(c(start:x))
  })
  names(bins) <- cols
  data_bins <- sapply(data_years, function(x){
    i <- sapply(bins, function(y){
      return(x %in% y)
    })
    return(names(bins[i]))
  })

  return(data_bins)

 }
