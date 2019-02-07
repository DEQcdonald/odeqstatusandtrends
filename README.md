# odeqstatusandtrends

install.packages("devtools")

## Install this package from GitHub
devtools::install_github("https://github.com/donco/odeqstatusandtrends")

## 1. Get station location information
To get the stations you'll use the GetStations() function which will require a shapefile of the area with which to do the analysis, and whether or not you would like to exclude tribal lands from the analysis. Optionally, you can tell the function what the name of your stations database connection is. The default is "STATIONS".

example: GetStations(polygon = "your-shapefile-here", exclude.tribal.lands = TRUE, stations.channel.name = "STATIONS")

This will return the raw station data from the stations database containing all of the stations within your desired area.

## 2. Get water quality monitoring data

Make sure you have the AWQMSdata package installed, devtools::install_github("TravisPritchardODEQ/AWQMSdata").

Use the GetData() function to retrieve water quality monitoring data from the AWQMS database. This function requires the parameters you wish to query (temp, bacteria (ecoli, fecal coliform, enterococcus), DO, TSS, TP, and pH), the stations dataframe from GetStations(), and the start and end date (YYYY-MM-DD, %Y-%m-%d) of your desired timeframe.

This will return the raw data retrieved from AWQMS using the AWQMS_Data() function.

example: GetData(parameters = c("parameter1", "parameter2"), stations = result-of-GetStations(), start.date = "2010-01-01", end.date = "2019-01-01", awqms.channel.name = "AWQMS")

## 3. Clean data for analysis

Use the CleanData() function to remove unnecessary variable, check the data for quality, and add the datetime and sample id columns for future use. This function only requires the output from GetData().
