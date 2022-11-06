###################################################################
# Function: forecast_conditional_var
# Author: Max Sties
# 
# Wrapper function for predict.varest that does stepwise VAR forecasts
# for a forecast horizon h. It allows for the forecast to be conditioned
# on specific paths or values for some of the endogenous variables. The
# function returns a forecast object that is missing prediction intervals.
# For conditional VAR forecasts with valid prediction intervals see the 
# BVAR packages.
# 
# NOTE: Both, the data with the external forecast values as well as the 
# data in the original VAR estimate need to be of class 'mtx' i.e. time
# series matrix.
###################################################################

forecast_conditional_var2 <- function(model, h, mat.fc.external){
  
  require(forecast)
  #require(lubridate)
  
  
  ## 1. Input Checks ----
  
  # Check model
  if(class(model) != "varest") 
    stop('This methods only works for models of  class "varest" 
       availabe with the VARS package')
  # Check mat.fc.external
  # class
  if(!any(class(mat.fc.external) == "mts")) 
    stop('External forecast paths need to be supplied in matrix form with 
         missing values (NA) for all data points to be forecasted and numeric 
         values for all known values.')
  # frequency
  if(!frequency(mat.fc.external) == frequency(model$y))
    stop('External forecast matrix needs to have the same frequency as the 
           data in the supplied estimated model')
  # dimension
  if(!all(dim(mat.fc.external) == c(h, ncol(model$y))))
    stop('External forecast matrix needs to have the same number of rows a 
         the forecast horizon (h) and the same number of columns as the data
         in the supplied estimated model')      
  # colnames
  if(!all(colnames(mat.fc.external) == colnames(model$y)))
    stop('Column names of the external forecast matrix need to have the same
         column names as the data in the supplied estimated model')
  
  
  ## 2. Set Up ----
  
  data <- model$y
  varnames <- colnames(data)
  
  start.data <- start(data)
  end.data <- end(data)
  start.fc <- start(mat.fc.external )
  end.fc <- end(mat.fc.external )
  
  fc <- forecast(model, h)
  
  
  ## 3. Forecast Loop ----
  # loop for point forecasts
  for (i in 1:h) {
    # Forecast One Step Ahead
    fc.i <- forecast(model, 1)$forecast
    means <- sapply(fc.i, function(x) x['mean'])
    names(means) <- colnames(data)
    newrow <- unlist(means)
    
    # Fill in external values for that period
    ind.external.i <- !is.na(mat.fc.external [i,]) # index of non-NA values
    newrow[ind.external.i] <-mat.fc.external [i, ind.external.i]
    
    # Update Data
    data <- ts(rbind(data, newrow), start = start.data, freq = frequency(data))
    
    # Re-Estimate Model
    model <- update(model, y = data)
    
  }  
  # loop for upper bound
  for (i in 1:h) {
    # Forecast One Step Ahead
    fc.i <- forecast(model, 1)$forecast
    means <- sapply(fc.i, function(x) x['upper'])
    names(means) <- colnames(data)
    newrow <- unlist(means)
    
    # Fill in external values for that period
    ind.external.i <- !is.na(mat.fc.external [i,]) # index of non-NA values
    newrow[ind.external.i] <-mat.fc.external [i, ind.external.i]
    
    # Update Data
    data <- ts(rbind(data, newrow), start = start.data, freq = frequency(data))
    
    # Re-Estimate Model
    model <- update(model, y = data)
    
  }  
  
  
  ## 4. Filling in the Results  
  
  for (i in seq_along(varnames)){
    #fc$forecast[[i]]$mean <- mat.fc[,i]
    fc$forecast[[i]]$mean <- window(data[, i], start = start.fc)
    fc$forecast[[i]]$upper[,] <- window(data[, i], start = start.fc)
    fc$forecast[[i]]$lower[,] <- NA
  }
  
  # add the matrix of external data to the results
  fc$mat.fc.external <- mat.fc.external
  
  return(fc)  
}


## Usage


library(vars)

# data(Canada)
# mod <- VAR(Canada, p = 2, type = 'const')
# horizon <- 8
# # create matrix of external forecast paths
#   mat.external <- matrix(NA, nrow = horizon, ncol = ncol(Canada))
#   colnames(mat.external) <- colnames(Canada)
#   # simulate increase in real wages
#   mat.external[,'rw'] <- seq(from = as.numeric(tail(Canada[,'rw'], n = 1)), 
#                              to = 500, length.out = horizon)
#   # we also already know the next value of unemployment
#   mat.external[1,'U'] <- 6.5
#   mat.external
#   # make it a time series matrix
#   mat.external <- ts(mat.external, start = c(2001, 1), freq = frequency(Canada))
#   mat.external
# # use the function
# fc.rw.rise <- forecast_conditional_var(model = mod, h = 8, 
#                                        mat.fc.external = mat.external)  
# 
# plot(fc.rw.rise)
# fc.rw.rise$mat.fc.external




