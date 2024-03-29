#######################################
# ECON 485
# Conditional VAR Forecasting using 1-step Ahead Forecast Iteration

# Initialize
rm(list = ls())
#dev.off(dev.list()["RStudioGD"])

#setwd("C:/Users/James/My Drive/R/gov_chall/The-Governor-s-Challenge")
#setwd("/Volumes/GoogleDrive/My Drive/R")
setwd("/Users/labuser/Desktop/temp_wd")

# Packages
library(cansim)       # Get data from StatsCan
library(fredr)        # Get data from FRED
library(forecast)     # Time Series Forecasting
library(vars)         # Vector Autoregression
library(lubridate)    # Easy date conversions
library(openxlsx)
library(gdata)
library(stringr)
library(xts)
library(tsbox)
library(tseries)
library(dplyr)


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

# Reading in Data

cpi <- ts_cansim("v41690914", start = date.start)
gdp.quart <- ts_cansim("v62305752", start = date.start)
target.d <- ts_cansim("v39079", start = date.start)    
u <- ts_cansim("v2062815", start = date.start)

# quarterly gdp component data
gdp.q <- ts_cansim("v62305752", start = date.start)     # gdp, quarterly, SA, 2012=100
hh.cons <- ts_cansim("v62305724", start = date.start)   # households' final consumption expenditure, quarterly, SA, 2012=100
nphh.cons <- ts_cansim("v62305723", start = date.start) # non-profits serving households' consumption expenditure, quarterly, SA, 2012=100
govt.cons <- ts_cansim("v62305731", start = date.start) # governments final consumption expenditure, quarterly, SA, 2012=100
bus.cap <- ts_cansim("v62305733", start = date.start)   # business gross fixed capital formation, quarterly, SA, 2012=100
govt.cap <- ts_cansim("v62305740", start = date.start)  # government gross fixed capital formation, quarterly, SA, 2012=100
nphh.cap <- ts_cansim("v62305739", start = date.start)  # non-profits serving households' fixed capital formation, quarterly, SA, 2012=100
bus.invrs <- ts_cansim("v62305741", start = date.start) # investment in inventories, quarterly, SA, 2012=100
xprts <- ts_cansim("v62305745", start = date.start)     # exports, quarterly, SA, 2012=100
mprts <- ts_cansim("v62305748", start = date.start)     # imports, quarterly, SA, 2012=100


# create gdp component variables
con <- hh.cons + nphh.cons + nphh.cons
gov <- govt.cons + govt.cap 
inv <- bus.cap + bus.invrs + nphh.cap
nex <- xprts - mprts



fredr_set_key('')
wti <- ts_fred('MCOILWTICO', start = date.start) # WTI oil price


url.usgdp <- "https://cdn.ihsmarkit.com/www/default/1020/US-Monthly-GDP-History-Data.xlsx"
df.usgdp <- read.xlsx(url.usgdp, sheet = 'Data')
gdp.us <- ts(df.usgdp$Monthly.Real.GDP.Index, start = c(1992, 1), freq = 12)

###################################### 

# Variable Transformations

## Converting daily target rate to monthly
# convert to xts object
seq.time <- seq(as.Date(date.start), by = 'day', length.out =  length(target.d))
target.xts.d <- xts(as.numeric(target.d), seq.time)
# create the most frequent value aggregation function
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
# aggregate
target.xts <- aggregate(target.xts.d, as.yearqtr, calculate_mode)
#target.xts <- apply.monthly(target.xts.d, calculate_mode) # Leave as rate
target <- as.ts(target.xts)

###################################### 
# Change frequency to quarterly

monthly <- ts(cpi, start = c(1993, 1), frequency = 12)
cpi.q <- aggregate(monthly, nfrequency = 4, mean)

monthly <- ts(u, start = c(1993, 1), frequency = 12)
unemp.q <- aggregate(monthly, nfrequency = 4, mean)

monthly <- ts(gdp.us, start = c(1992, 1), frequency = 12)
gdp.us.q <- aggregate(monthly, nfrequency = 4, mean)

monthly <- ts(wti, start = c(1993, 1), frequency = 12)
wti.q <- aggregate(monthly, nfrequency = 4, mean)

###################################### 
## Making Series Stationary
GDP <- diff(log(gdp.quart), 4)       # Convert to yoy GDP growth
CON <- diff(log(con), 4)
INV <- diff(log(inv), 4)
XPRTS <- diff(log(xprts), 4)
#GDP <- ((gdp.quart - lag(gdp.quart)) / lag(gdp.quart)) * 100
INF <- diff(log(cpi.q), 4)           # Convert to yoy inflation
#INF <- diff(INF)
U <- unemp.q                         # Leave as rate
#U <- diff(U) # taking first difference
TARGET <- target                     # Leave as rate
GDP.US <- diff(log(gdp.us.q), 4)     # Convert to yoy GDP growth
WTI <- wti.q                         # Leave as price level

adf.test(GDP) # fail p-value = 0.1613
ndiffs(GDP) # says not stationary
plot(GDP)
adf.test(INF) # fail p-value = 0.8207
ndiffs(INF, test = "adf") # says stationary..
plot(INF) 
adf.test(U) # pass p-value = 0.03484
ndiffs(U) # says not stationary..
adf.test(TARGET) # fail p-value = 0.05677
ndiffs(TARGET) # says not stationary
plot(TARGET)
adf.test(GDP.US) # pass p-value = 0.03655
ndiffs(GDP.US)

# rotating components
adf.test(CON) 
ndiffs(CON)
plot(CON)
adf.test(INV) 
ndiffs(INV)
plot(INV)
adf.test(XPRTS)
ndiffs(XPRTS)
plot(XPRTS)


#adf.test(WTI) # fail, ignore since not using
#plot(WTI)

###################################### 
# Combining all Series in Time Series Matrix
data <- cbind(INF, GDP, U, TARGET, GDP.US)
data.cons <- cbind(INF, CON, U, TARGET, GDP.US)
data.inv <- cbind(INF, INV, U, TARGET, GDP.US)
data.xprts <- cbind(INF, XPRTS, U, TARGET, GDP.US)
#data <- cbind(INF, GDP, U, TARGET, GDP.US, WTI)

###################################### 
# set data based on desired forecast

data <- data.cons
data <- data.inv
data <- data.xprts

plot(data)

# Complete Data Set

head(data, 24)
tail(data)

ind.complete <- complete.cases(data)
# Need to use window command to preserve time series character
date.complete <- yearqtr(time(data)[ind.complete])
date.complete.start <- yearqtr(min(date.complete))
date.complete.end <- yearqtr(max(date.complete))

data.complete <- window(data, start = c(year(date.complete.start), quarter(date.complete.start)), 
                        end = c(year(date.complete.end), quarter(date.complete.end)))

plot(data.complete)
any(is.na(data.complete))
tail(data.complete, 12)

###################################### 
## 2. Estimate Model ----


# 2.1 Lag Selection
VARselect(data.complete, 4)
n.lag <- 1

mod.est <- VAR(data.complete, p = n.lag)

# 2.2 Restrict Coefficients

mat.coef <- sapply(coef(mod.est), function(x) x[,'Estimate'])
mat.coef

# Need the transpose of this matrix to get the dimensions right for our restrictions matrix
mat.coef.res <- t(mat.coef)
mat.coef.res[,] <- 1

# Impose the restrictions 
mat.coef.res
mat.coef.res['GDP.US', 1:4] <- 0 
#mat.coef.res[c('WTI','GDP.US'), 1:4] <- 0     # 1. Lag
#mat.coef.res[c('WTI','GDP.US'), 7:10] <- 0    # 2. Lag
#mat.coef.res[c('WTI','GDP.US'), 13:16] <- 0   # 3. Lag
#mat.coef.res[c('WTI','GDP.US'), 19:22] <- 0   # 4. Lag
mat.coef.res



# 2.3 Re-estimate Model
mod.restrict <- restrict(mod.est, method = "man", resmat = mat.coef.res)
coef(mod.restrict)

# 2.4 Granger Causality Tests
causality(mod.restrict, cause = 'INF') 
causality(mod.restrict, cause = 'GDP') 
causality(mod.restrict, cause = 'U') 
causality(mod.restrict, cause = 'TARGET') 
#causality(mod.restrict, cause = 'WTI') 
causality(mod.restrict, cause = 'GDP.US') 

# stability
roots(mod.restrict) 


## Iterative Forecast ----


## et Up
h = 10

date.fc.start <- date.complete.end + 3/12
date.fc.end <- date.complete.end + h/4

varnames <- colnames(data.complete)

# Look at unconditional Forecast
fc.uncond <- forecast(mod.restrict, h = h)
plot(fc.uncond, include = 60)
fc.uncond

## create Time Series Matrix with known values

data.fc <- window(data, start = c(year(date.fc.start), quarter(date.fc.start)), 
                  end = c(year(date.fc.end), quarter(date.fc.end)), 
                  extend = TRUE) # 'extend' allows the window to be larger than the data 


## 2.3 Fill in Paths of Conditioned on Variables (Play around with these)

window(data.fc[, "TARGET"], start = c(2022, 4), end = c(2023,4)) <-
  c(4, 3.75, 3.5, 3.25, 3)

#window(data.fc[, "GDP.US"], start = c(2022, 4), end = c(2024,4)) <- 
#  c(-0.01, -0.02, -0.01, -0.05, 0, 0.25, 0.5, 0.75, 1)

# Check 
data.fc 

## Iterative Forecast

fc <- forecast_conditional_var(mod.restrict, h, data.fc)


## Plot Forecast and Overlay Imposed Values

plot(fc, include = 32)
fc

# restore inflation

autoplot(fc, colour = FALSE,  showgap = TRUE)  +
  autolayer(data.fc, color = 'red')

autoplot(fc$forecast$INF, include = 32) + 
  autolayer(data.fc[,'INF'], color = 'magenta')

autoplot(fc$forecast$CON, include = 32) + 
  autolayer(data.fc[,'CON'], color = 'magenta')

autoplot(fc$forecast$INV, include = 32) + 
  autolayer(data.fc[,'INV'], color = 'magenta')

autoplot(fc$forecast$XPRTS, include = 32) + 
  autolayer(data.fc[,'XPRTS'], color = 'magenta')

fc$forecast

INF.fc <- ts.union(INF, fc$forecast$INF$mean)
GDP.fc <- ts.union(GDP, fc$forecast$GDP$mean)
U.fc <- ts.union(U, fc$forecast$U$mean)
TARGET.fc <- ts.union(TARGET, fc$forecast$TARGET$mean)

df1 <- data.frame(cbind(DATE = as.yearqtr(time(INF.fc)), INF = INF.fc, GDP = GDP.fc, U = U.fc,
                               TARGET = TARGET.fc))

df2 <- data.frame(ts.union(INF = INF.fc, GDP = GDP.fc, U = U.fc,
                        TARGET = TARGET.fc))

#write.xlsx(df1, "output/base_components1.xlsx")
