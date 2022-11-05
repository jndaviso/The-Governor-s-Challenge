#######################################
# ECON 485
# Conditional VAR Forecasting using 1-step Ahead Forecast Iteration

# Initialize
rm(list = ls())
#dev.off(dev.list()["RStudioGD"])

#setwd("G:/My Drive/Studies/Teaching/Classes/Econ 485/2022/R")
setwd("C:/Users/James/My Drive/R/gov_chall/The-Governor-s-Challenge")
#setwd("E:/Google Drive/Studies/Teaching/Classes/Econ 485/2022/R")

# Packages
library(cansim)       # Get data from StatsCan
library(fredr)        # Get data from FRED
library(forecast)     # Time Series Forecasting
library(vars)         # Vector Autoregression
library(lubridate)    # Easy date conversions
library(openxlsx)
library(stringr)
library(xts)
library(tsbox)

# Functions
source("functions/ts_cansim.R")
source("functions/ts_fred.R")
source("functions/ts_oecd.R")
source("functions/forecast_conditional_var.R")

######################################
## Parameters

date.start <- '1993-01-01'


###################################### 

## 1. Data ----


# 1.1 Reading in Data
# You should document where your data comes from (not like me here!)
cpi <- ts_cansim("v41690914", start = date.start)
gdp <- ts_cansim("v65201210", start = date.start)
gdp.old <- ts_cansim("v41881478", start = date.start)  # GDP with older base year (terminated)
ten.year <- ts_cansim("v122543", start = date.start)
wage <- ts_cansim("v103451670", start = date.start)
target.d <- ts_cansim("v39079", start = date.start)    # daily target rate 
u <- ts_cansim("v2062815", start = date.start)

wti <- ts_fred('MCOILWTICO', start = date.start) # WTI oil price

# Get Monthly US GDP
## Monthly US GDP estimates from: https://ihsmarkit.com/products/us-monthly-gdp-index.html
url.usgdp <- "https://cdn.ihsmarkit.com/www/default/1020/US-Monthly-GDP-History-Data.xlsx"
df.usgdp <- read.xlsx(url.usgdp, sheet = 'Data')
gdp.us <- ts(df.usgdp$Monthly.Real.GDP.Index, start = c(1992, 1), freq = 12)

# 1.2 Variable Transformations

## a) Converting daily target rate to monthly
# convert to xts object
seq.time <- seq(as.Date(date.start), by = 'day', length.out =  length(target.d))
target.xts.d <- xts(as.numeric(target.d), seq.time)
# create the most frequent value aggregation function
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
# aggregate
target.xts <- aggregate(target.xts.d, as.yearmon, calculate_mode)
#target.xts <- apply.monthly(target.xts.d, calculate_mode) # Leave as rate
target <- as.ts(target.xts)

## b) Splicing Monthly GDP data together
plot(cbind(gdp,gdp.old))
# regress gdp.old on gdp.new
ind.overlap <- complete.cases(cbind(gdp, gdp.old))
ind.predict <- is.na(cbind(gdp, gdp.old)[,'gdp'])
df.overlap <- as.data.frame(cbind(gdp, gdp.old)[ind.overlap,])
df.predict <- as.data.frame(cbind(gdp, gdp.old)[ind.predict,])
mod.splice <- lm(gdp ~ gdp.old, data = df.overlap)
# predict new values for old dates
gdp.hat <- predict(mod.splice, newdata = df.predict)
gdp.splice <- ts(c(gdp.hat, gdp), start = c(year(date.start),month(date.start)), freq = 12)
# check
plot(gdp.splice)
lines(gdp, col = 'red')
# looks good

## c) Making Series Stationary
GDP <- diff(log(gdp.splice), 12)       # Convert to yoy GDP growth
INF <- diff(log(cpi), 12)       # Convert to yoy inflation
U <- u                          # Leave as rate
TARGET <- target                # Leave as rate
GDP.US <- diff(log(gdp.us), 12) # Convert to yoy GDP growth
WTI <- wti                      # Leave as price level

# d) Combining all Series in Time Series Matrix
data <- cbind(INF, GDP, U, TARGET, GDP.US, WTI)
plot(data)


# 1.3 Find Complete Data Set

head(data, 24)
tail(data)

ind.complete <- complete.cases(data)
# Need to use window command to preserve time series character
date.complete <- yearmon(time(data)[ind.complete])
date.complete.start <- min(date.complete)
date.complete.end <- max(date.complete)

data.complete <- window(data, start = c(year(date.complete.start), month(date.complete.start)), 
                        end = c(year(date.complete.end), month(date.complete.end))
)

plot(data.complete)
any(is.na(data.complete))



## 2. Estimate Model ----


# 2.1 Lag Selection
VARselect(data.complete, 10)
n.lag <- 3

mod.est <- VAR(data.complete, p = n.lag)

# 2.2 Restrict Coefficients

mat.coef <- sapply(coef(mod.est), function(x) x[,'Estimate'])
mat.coef

# Need the transpose of this matrix to get the dimensions right for our restrictions matrix
mat.coef.res <- t(mat.coef)
mat.coef.res[,] <- 1

# Impose the restrictions (This is going to change with different lag length!!!)
mat.coef.res
mat.coef.res[c('WTI','GDP.US'), 1:4] <- 0     # 1. Lag
mat.coef.res[c('WTI','GDP.US'), 7:10] <- 0    # 2. Lag
mat.coef.res[c('WTI','GDP.US'), 13:16] <- 0   # 3. Lag
mat.coef.res
# Note: We are allowing US GDP and the oil price to affect each other


# 2.3 Re-estimate Model
mod.restrict <- restrict(mod.est, method = "man", resmat = mat.coef.res)
coef(mod.restrict)

# 2.4 Granger Causality Tests
causality(mod.restrict, cause = 'INF')
causality(mod.restrict, cause = 'GDP')
causality(mod.restrict, cause = 'U')
causality(mod.restrict, cause = 'TARGET')
causality(mod.restrict, cause = 'WTI')
causality(mod.restrict, cause = 'GDP.US')

# 2.4 Stability
roots(mod.restrict)
# Not good, not terrible...



## 3. Iterative Forecast ----


## 3.1 Set Up
h = 4 + 12 + 12

date.fc.start <- date.complete.end + 1/12
date.fc.end <- date.complete.end + h/12

varnames <- colnames(data.complete)

# Look at unconditional Forecast
fc.uncond <- forecast(mod.restrict, h = h)
plot(fc.uncond, include = 60)


## 2.2 Create Time Series Matrix with known values

data.fc <- window(data, start = c(year(date.fc.start), month(date.fc.start)), 
                  end = c(year(date.fc.end), month(date.fc.end)), 
                  extend = TRUE) # 'extend' allows the window to be larger than the data 


## 2.3 Fill in Paths of Conditioned on Variables (Play around with these)

# window(data.fc[, "TARGET"], start = c(2022, 12), end = c(2023, 4)) <-
#  c(rep(3.75, 2), rep(4, 3))

window(data.fc[, "TARGET"], start = c(2022, 12), end = c(2023,12)) <-
   c(rep(3.75, 2), rep(4, 3), rep(4.25, 2), rep(4, 2), rep(3.75, 2), rep(3.5, 2))

window(data.fc[, "WTI"], start = c(2022, 11), end = c(2023,12)) <- 
   rep(90, 14)

window(data.fc[, "GDP.US"], start = c(2022, 9), end = c(2023,6)) <- 
  rep(c(0.02, 0.01, 0.005, 0, -0.005, -0.01, -0.015,-0.005, 0, 0.005))


data.fc # Check to see that your imposed values have the right scale and timing


## 2.4 Use Function for Iterative Forecast

fc <- forecast_conditional_var(mod.restrict, h, data.fc)


## 2.5 Plot Forecast and Overlay Imposed Values

plot(fc, include = 32)

autoplot(fc, colour = FALSE,  showgap = TRUE)  +
  autolayer(data.fc, color = 'red')

autoplot(fc$forecast$TARGET, include = 32) + 
  autolayer(data.fc[,'TARGET'], color = 'magenta')

autoplot(fc$forecast$GDP, include = 32) + 
  autolayer(data.fc[,'GDP'], color = 'magenta')

fc$forecast
