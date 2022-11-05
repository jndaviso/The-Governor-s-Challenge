# ts_oecd
# uses get_dataset from the OECD packages to read in data and convert them into a time series
# Code by Max Sties
# Note: Work in progress


ts_oecd <- function(dataset , apikey, plotit = FALSE){
  
  require(OECD)
  require(tidyr)
  require(dplyr)
  require(tsbox)
  require(xts)  
  
  data.oecd.gdp.q <- get_dataset(dataset, apikey)
  # Select only relevant columns 
  oecd.small <- select(data.oecd.gdp.q, LOCATION, ObsValue, Time)
  # Convert observations from character to numeric
  oecd.small$ObsValue <- as.numeric(oecd.small$ObsValue)
  # reshape to countries in column
  oecd.wide <- pivot_wider(oecd.small, names_from = LOCATION, values_from = ObsValue)
  # fix date to proper format
  
  if (data.oecd.gdp.q$FREQUENCY[1] == 'M'){
    oecd.wide$Time <- as.Date(as.yearmon(oecd.wide$Time, format = "%Y-%m"))
  }
  else if (data.oecd.gdp.q$FREQUENCY[1] == 'Q'){
    oecd.wide$Time <- as.Date(as.yearqtr(oecd.wide$Time, format = "%Y-Q%q"))
  }
  else {
    print('current frequency not available')
  }
  # convert to time series matrix
  oecd.xts <- xts(oecd.wide[,-1], order.by = oecd.wide$Time)
  oecd.ts <- ts_ts(oecd.xts)
  if (plotit == TRUE){plot(oecd.ts)}
  return(oecd.ts)
}

# usage:
# dataset <- "QNA"
# apikey <- 'CAN+JPN+GBR+USA+EU15+NMEC+BRA+CHN+IND.B1_GE.GYSA.Q/all?startTime=1997-Q1&endTime=2022-Q4&dimensionAtObservation=allDimensions'
# dataset <- "STLABOUR"
# apikey <- 'CAN+JPN+MEX+GBR+USA+EA19+EU27_2020.LRHUTTTT.STSA.M/all?startTime=1997-01&endTime=2022-08&dimensionAtObservation=allDimensions'
# xx <- ts_oecd(dataset, apikey, plotit = TRUE)
# class(xx)
# xx
# yy <- ts_oecd(dataset, apikey)
# yy