% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_temperature.R
\name{plot_temperature}
\alias{plot_temperature}
\title{Create temperature plots to display status and trend.}
\usage{
plot_temperature(
  data,
  seaKen,
  station,
  max_date = min(data$sample_datetime, na.rm = TRUE)
)
}
\arguments{
\item{data}{Dataframe to determine status from. Must have 'excursion' column generated.}

\item{seaKen}{Results of Seasonal Kendall Analysis}

\item{station}{The station to plot}

\item{max_date}{The max date to show on the plot}
}
\value{
dataframe of stations with sufficient data
}
\description{
Create temperature plots to display status and trend.
}
\examples{
plot_temperature(data = data.frame, seaKen, station)
}
