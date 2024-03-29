% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Locations.R
\name{get_stations_WQP}
\alias{get_stations_WQP}
\title{Get Water Quality Portal stations in Oregon for status and trends analysis}
\usage{
get_stations_WQP(
  polygon,
  start_date,
  end_date,
  huc8,
  exclude.tribal.lands = TRUE
)
}
\arguments{
\item{polygon}{Shapefile of the area to query}

\item{start_date}{Date to begin query}

\item{end_date}{Date to end query}

\item{exclude.tribal.lands}{Whether or not to exclude stations located on tribal lands. Defaults to TRUE.}
}
\value{
A list of stations within a given shapefile.
}
\description{
Queries the Water Quality Portal stations database to pull all available stations within a given shapefile, ignoring OREGONDEQ orgIDs.
}
\examples{
get_stations_AWQMS(polygon = "your-shapefile-here", exclude.tribal.lands = TRUE, start_date, end_date)
}
