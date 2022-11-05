#####
## ECON 485 - Group 1 - Forecasting Code v.4 Final
# Now with exogenous supply chains!

## Initialize
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# lab wd
# setwd("/Users/labuser/Desktop/temp_wd")

# desktop gdrive wd
setwd("C:/Users/James/My Drive/a.Classes/Fall 2022/ECON 485/R Forecasting/temp_wd")
# mac gdrive wd

# Packages
library(lubridate)    # Easy date conversions
library(cansim)       # Get data from StatsCan
library(fredr)        # Get data from FRED
#####
# a) data read-in and adjustments     # Time Series Forecasting
library(vars)         # Vector Autoregression
library(plotrix)      # Draw a circle
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(writexl)
library(tseries)


# Functions
source("functions/ts_cansim.R")
source("functions/ts_fred.R")
source("functions/ts_oecd.R")

#####
# data read-in and adjustments

date.start <- '1993-01-01'

# monthly data
cpi <- ts_cansim("v41690914", start = date.start) # cpi, monthly, SA, 2002=100
gdp.m <- ts_cansim("v65201210", start = date.start) # gdp, monthly, SA, 2012=100
unemp.rt <- ts_cansim("v2062815", start = date.start) # monthly, unemployment rate, SA
wage.rt <- ts_cansim("v2132579", start = date.start) # average hourly wage, monthly, not SA, 2022 dollars
bank.rt <- ts_cansim("v122530", start = date.start) # bank rate, monthly
trgt.rt <- bank.rt - 0.25
#five.yr <- ts_cansim("v39053", start = date.start) # GoC benchmark bond yields, 5 year 
#wage <- ts_cansim("v2132579", start = date.start) # average hourly wage, monthly, not SA, 2022 dollars

us.gdp <- ts(data.frame(read.csv(file = "data/realgdpindex_ihsmarkit.csv"))$value, start = c(1992, 01), frequency = 12) # read in csv for us monthly gdp,, no SA
gscpi <- ts(data.frame(read.csv(file = "data/gscpi.csv"))$value, start = c(1998, 01), frequency = 12) # read in gscppi
fredr_set_key('7eb7c5788c21aacbba171d29b877f086')
wti <- ts_fred('MCOILWTICO', start = date.start) # oil price, monthly, no SA

# quarterly data
gdp.q <- ts_cansim("v62305752", start = date.start) # gdp, quarterly, SA, 2012=100
hh.cons <- ts_cansim("v62305724", start = date.start) # households' final consumption expenditure, quarterly, SA, 2012=100
nphh.cons <- ts_cansim("v62305723", start = date.start) # non-profits serving households' consumption expenditure, quarterly, SA, 2012=100
govt.cons <- ts_cansim("v62305731", start = date.start) # governments final consumption expenditure, quarterly, SA, 2012=100
bus.cap <- ts_cansim("v62305733", start = date.start) # business gross fixed capital formation, quarterly, SA, 2012=100
govt.cap <- ts_cansim("v62305740", start = date.start) # government gross fixed capital formation, quarterly, SA, 2012=100
nphh.cap <- ts_cansim("v62305739", start = date.start) # non-profits serving households' fixed capital formation, quarterly, SA, 2012=100
bus.invrs <- ts_cansim("v62305741", start = date.start) # investment in inventories, quarterly, SA, 2012=100
xprts <- ts_cansim("v62305745", start = date.start) # exports, quarterly, SA, 2012=100
mprts <- ts_cansim("v62305748", start = date.start) # imports, quarterly, SA, 2012=100
#capu.rt <- ts_cansim("v4331081", start = date.start) # Total Industrial Capacity Utilization rate, quarterly 

# create gdp component variables
con <- hh.cons + nphh.cons + nphh.cons
gov <- govt.cons + govt.cap 
inv <- bus.cap + bus.invrs + nphh.cap
nex <- xprts - mprts


###
# checking for and making stationary
# *part of section excised, see v.3*

adf.test(unemp.rt) # is stat. at 10%

adf.test(trgt.rt) # is stat.

CPI <- diff(log(cpi), 12)
adf.test(CPI)

GDP.M <- diff(log(gdp.m), 12)
adf.test(GDP.M)

#UNEMP <- diff(unemp.rt, 12)
UNEMP <- diff(unemp.rt, 1)
#UNEMP <- unemp.rt
adf.test(UNEMP) # is stat.

# TRGT <- diff(trgt.rt, 12)
TRGT <- trgt.rt
# TRGT <- diff(trgt.rt, 12, 2)
adf.test(trgt.rt)

X.USGDP <- diff(log(us.gdp), 12)
adf.test(X.USGDP)

adf.test(gscpi) # not stat.
#X.GSCPI <- diff(gscpi, 1) # first diff
#adf.test(X.GSCPI) # is stat.
X.GSCPI <- gscpi
#adf.test(wti)
#WTI <- diff(log(()))
#plot(wti)


#####
# Filling all Missing Values with Iterative One-Step Ahead VAR 
# for CPI, GDP.M, UNEMP, TRGT, X.USGDP, X.WTI

# construct the restriction matrix
data.mainx <- cbind(CPI, GDP.M, UNEMP)

date.est.start <- c(1998, 2)

exomat <- cbind(TRGT, X.GSCPI, X.USGDP)
tail(exomat)
head(exomat)

# combine in window
data.exo <- window(cbind(data.mainx, exomat), 
                   start = date.est.start)

colnames(data.exo) <- c('CPI', 'GDP.M', 'UNEMP', 
                        'TRGT', 'X.GSCPI', 'X.USGDP')
head(data.exo)
tail(data.exo)

# create estimation window from complete data
date.est.end <- c(2022, 8)
data.exo.aug <- window(data.exo, start = date.est.start, end = date.est.end)

tail(data.exo.aug)

# estimate the model without restrictions
VARselect(data.exo.aug, 6)
# Selects a lag length of 3
lags <- 4
mod.est <- VAR(data.exo.aug, p = lags)
coef(mod.est)

# get the coefficient matrix to put in the restrictions
mat.coef <- sapply(coef(mod.est), function(x) x[,'Estimate'])

# Need the transpose of this matrix to get the dimensions right for our restrictions matrix
mat.coef.res <- t(mat.coef)
mat.coef.res[,] <- 1

# Impose the restrictions (This is going to change with different lag length!!!)
mat.coef.res
mat.coef.res[c('X.GSCPI', 'X.USGDP'), 1:4] <- 0
mat.coef.res[c('X.GSCPI', 'X.USGDP'), 7:10] <- 0
mat.coef.res[c('X.GSCPI', 'X.USGDP'), 13:16] <- 0
mat.coef.res

# Re-estimate the model with the restrictions
mod.est.restrict <- restrict(mod.est, method = "man", resmat = mat.coef.res)

coef(mod.est.restrict)

# Granger Causality Testing
causality(mod.est.restrict, cause = 'X.GSCPI')
causality(mod.est.restrict, cause = 'X.USGDP')
# both pass

# Stability
roots(mod.est.restrict)
# unit root present if using base unemployment, using first diff now
# unit root gone

#
# Iterative Forecasting to fill in missing values

tail(data.exo)

# September missing GDP
# forecast September value
mod.restrict.fc <- forecast(mod.est.restrict, h = 1)
plot(mod.restrict.fc, include = 36)
# expand the data by one month
data.exo.sep <- window(data.exo, start = date.est.start, end = c(2022,9))

tail(data.exo.sep)

# input the forecast value to the dataset
n.sep <- nrow(data.exo.sep)
data.exo.sep[n.sep, 'GDP.M'] <- mod.restrict.fc$forecast$GDP.M$mean[1]
data.exo[n.sep, 'GDP.M'] <- mod.restrict.fc$forecast$GDP.M$mean[1]
data.exo.sep[n.sep, 'X.USGDP'] <- mod.restrict.fc$forecast$X.USGDP$mean[1]
data.exo[n.sep, 'X.USGDP'] <- mod.restrict.fc$forecast$X.USGDP$mean[1]
tail(data.exo)


#####
# Monthly VAR Model and Forecast

# create dataset with endo variables only
data.endo <- data.exo[ , 1:3]

# create exo matrix 
exomat <- cbind(data.exo[ , 4], data.exo[ , 5], data.exo[ , 6])
colnames(exomat) <- c('TRGT', 'X.GSCPI', 'X.USGDP')

tail(exomat)
tail(data.endo)

VARselect(data.endo, lag.max = 6, type = "const", exogen = exomat)

n.lag <- 3
mod.var <- VAR(data.endo,  p = n.lag, type = c("const"), exogen = exomat)
coef(mod.var)
roots(mod.var)

causality(mod.var, cause = "CPI") # fail
causality(mod.var, cause = "GDP.M") # pass
causality(mod.var, cause = "UNEMP") # fail

# exo paths
TRGT.path <- newbase.ts
X.GSCPI <- base.gscpi
X.USGDP <- base_recession_monthly

# path matrix
exomat.path <- cbind(TRGT.path, X.GSCPI, X.USGDP)
colnames(exomat.path) <- c('TRGT', 'X.GSCPI', 'X.USGDP')

# forecast
fc.var <- forecast(mod.var, h = nrow(exomat.path), dumvar = exomat.path)
plot(fc.var, include = 36)

fc.var$model$varresult

# GDP growth forecast
GDP.M.FC <- ts(c(GDP.M,fc.var$forecast$GDP.M$mean), start = start(GDP.M), frequency = frequency(GDP.M))
plot(GDP.M.FC)
lines(GDP.M, col = 'blue')

# CPI inflation forecast
CPI.FC <- ts(c(CPI,fc.var$forecast$CPI$mean), start = start(CPI), frequency = frequency(CPI))
plot(CPI.FC)
lines(CPI, col = 'blue')

# Unemployment rate forecast
UNEMP.FC <- ts(c(UNEMP,fc.var$forecast$UNEMP$mean), start = start(UNEMP), frequency = frequency(UNEMP))
unemp.fc <- diffinv(UNEMP.FC, differences = 1, xi = 11.2)

unemp.diff <- c(UNEMP,fc.var$forecast$UNEMP$mean)
unemp.fc <- ts(cumsum(c(unemp.rt[1], unemp.diff)), start = start(unemp.rt), freq = 12)
plot(unemp.fc)
lines(unemp.rt, col = 'blue')

unemp.hi.diff <- c(UNEMP,fc.var$forecast$UNEMP[["upper"]][,2])
unemp.hi.fc <- ts(cumsum(c(unemp.rt[1], unemp.hi.diff)), start = start(unemp.rt), freq = 12)

unemp.lo.diff <- c(UNEMP,fc.var$forecast$UNEMP[["lower"]][,2])
unemp.lo.fc <- ts(cumsum(c(unemp.rt[1], unemp.lo.diff)), start = start(unemp.rt), freq = 12)

plot(unemp.hi.fc)
lines(unemp.lo.fc)
lines(unemp.fc, col = 'blue')

length(unemp.fc)
393-36


# store in dataframe
var_ouput <- data.frame(date = as.Date(time(fc.var$forecast$GDP.M$mean)), 
                             gdp = fc.var$forecast$GDP.M$mean,
                        gdp.lo = fc.var$forecast$GDP.M[["lower"]][,2],
                        gdp.hi = fc.var$forecast$GDP.M[["upper"]][,2],
                             cpi = fc.var$forecast$CPI$mean,
                        cpi.lo = fc.var$forecast$CPI[["lower"]][,2],
                        cpi.hi = fc.var$forecast$CPI[["upper"]][,2],
                             unemp = unemp.fc[358:393],
                        unemp.lo = unemp.lo.fc[358:393],
                        unemp.hi = unemp.hi.fc[358:393],
                        exomat.path)


write_xlsx(var_ouput, "C:/Users/James/My Drive/a.Classes/Fall 2022/ECON 485/R Forecasting/temp_wd/Output/var_ouput3.xlsx")

cpi_inf <- data.frame(date = as.Date(time(CPI)), CPI)
write_xlsx(cpi_inf, "C:/Users/James/My Drive/a.Classes/Fall 2022/ECON 485/R Forecasting/temp_wd/Output/cpi_inf.xlsx")



#####
# Monthly VAR Model GSCPI Risk, US GDP mildly affected

# exo paths
R.TRGT.path <- newbase.ts
R.X.GSCPI <- risk.gscpi
R.X.USGDP <- suprisk_recession_monthly

# path matrix
exomat.path <- cbind(R.TRGT.path, R.X.GSCPI, R.X.USGDP)
colnames(exomat.path) <- c('TRGT', 'X.GSCPI', 'X.USGDP')

# forecast
fc.var <- forecast(mod.var, h = nrow(exomat.path), dumvar = exomat.path)
plot(fc.var, include = 36)

fc.var$model$varresult

# GDP growth forecast
GDP.M.FC <- ts(c(GDP.M,fc.var$forecast$GDP.M$mean), start = start(GDP.M), frequency = frequency(GDP.M))
plot(GDP.M.FC)
lines(GDP.M, col = 'blue')

# CPI inflation forecast
CPI.FC <- ts(c(CPI,fc.var$forecast$CPI$mean), start = start(CPI), frequency = frequency(CPI))
plot(CPI.FC)
lines(CPI, col = 'blue')

# Unemployment rate forecast
UNEMP.FC <- ts(c(UNEMP,fc.var$forecast$UNEMP$mean), start = start(UNEMP), frequency = frequency(UNEMP))
unemp.fc <- diffinv(UNEMP.FC, differences = 1, xi = 11.2)

unemp.diff <- c(UNEMP,fc.var$forecast$UNEMP$mean)
unemp.fc <- ts(cumsum(c(unemp.rt[1], unemp.diff)), start = start(unemp.rt), freq = 12)
plot(unemp.fc)
lines(unemp.rt, col = 'blue')

unemp.hi.diff <- c(UNEMP,fc.var$forecast$UNEMP[["upper"]][,2])
unemp.hi.fc <- ts(cumsum(c(unemp.rt[1], unemp.hi.diff)), start = start(unemp.rt), freq = 12)

unemp.lo.diff <- c(UNEMP,fc.var$forecast$UNEMP[["lower"]][,2])
unemp.lo.fc <- ts(cumsum(c(unemp.rt[1], unemp.lo.diff)), start = start(unemp.rt), freq = 12)

plot(unemp.hi.fc)
lines(unemp.lo.fc)
lines(unemp.fc, col = 'blue')

length(unemp.fc)
393-36


# store in dataframe
var_ouput <- data.frame(date = as.Date(time(fc.var$forecast$GDP.M$mean)), 
                        gdp = fc.var$forecast$GDP.M$mean,
                        gdp.lo = fc.var$forecast$GDP.M[["lower"]][,2],
                        gdp.hi = fc.var$forecast$GDP.M[["upper"]][,2],
                        cpi = fc.var$forecast$CPI$mean,
                        cpi.lo = fc.var$forecast$CPI[["lower"]][,2],
                        cpi.hi = fc.var$forecast$CPI[["upper"]][,2],
                        unemp = unemp.fc[358:393],
                        unemp.lo = unemp.lo.fc[358:393],
                        unemp.hi = unemp.hi.fc[358:393],
                        exomat.path)


write_xlsx(var_ouput, "C:/Users/James/My Drive/a.Classes/Fall 2022/ECON 485/R Forecasting/temp_wd/Output/var_ouput_gscpi_risk.xlsx")

#GDP.M <- data.frame(date = as.Date(time(GDP.M)), GDP.M)
#CPI <- data.frame(date = as.Date(time(CPI)), CPI)
#unemp <- data.frame(date = as.Date(time(unemp.rt)), unemp.rt)

#write_xlsx(GDP.M, "C:/Users/James/My Drive/a.Classes/Fall 2022/ECON 485/R Forecasting/temp_wd/Output/gdpm.xlsx")
#write_xlsx(CPI, "C:/Users/James/My Drive/a.Classes/Fall 2022/ECON 485/R Forecasting/temp_wd/Output/cpi.xlsx")
#write_xlsx(unemp, "C:/Users/James/My Drive/a.Classes/Fall 2022/ECON 485/R Forecasting/temp_wd/Output/unemprt.xlsx")

#####
# Monthly VAR Model US GDP Risk

# exo paths
R.TRGT.path <- newbase.ts
R.X.GSCPI <- base.gscpi
R.X.USGDP <- severe_recession_monthly

# path matrix
exomat.path <- cbind(R.TRGT.path, R.X.GSCPI, R.X.USGDP)
colnames(exomat.path) <- c('TRGT', 'X.GSCPI', 'X.USGDP')

# forecast
fc.var <- forecast(mod.var, h = nrow(exomat.path), dumvar = exomat.path)
plot(fc.var, include = 36)

fc.var$model$varresult

# GDP growth forecast
GDP.M.FC <- ts(c(GDP.M,fc.var$forecast$GDP.M$mean), start = start(GDP.M), frequency = frequency(GDP.M))
plot(GDP.M.FC)
lines(GDP.M, col = 'blue')

# CPI inflation forecast
CPI.FC <- ts(c(CPI,fc.var$forecast$CPI$mean), start = start(CPI), frequency = frequency(CPI))
plot(CPI.FC)
lines(CPI, col = 'blue')

# Unemployment rate forecast
UNEMP.FC <- ts(c(UNEMP,fc.var$forecast$UNEMP$mean), start = start(UNEMP), frequency = frequency(UNEMP))
unemp.fc <- diffinv(UNEMP.FC, differences = 1, xi = 11.2)

unemp.diff <- c(UNEMP,fc.var$forecast$UNEMP$mean)
unemp.fc <- ts(cumsum(c(unemp.rt[1], unemp.diff)), start = start(unemp.rt), freq = 12)
plot(unemp.fc)
lines(unemp.rt, col = 'blue')

unemp.hi.diff <- c(UNEMP,fc.var$forecast$UNEMP[["upper"]][,2])
unemp.hi.fc <- ts(cumsum(c(unemp.rt[1], unemp.hi.diff)), start = start(unemp.rt), freq = 12)

unemp.lo.diff <- c(UNEMP,fc.var$forecast$UNEMP[["lower"]][,2])
unemp.lo.fc <- ts(cumsum(c(unemp.rt[1], unemp.lo.diff)), start = start(unemp.rt), freq = 12)

plot(unemp.hi.fc)
lines(unemp.lo.fc)
lines(unemp.fc, col = 'blue')

length(unemp.fc)
393-36


# store in dataframe
var_ouput <- data.frame(date = as.Date(time(fc.var$forecast$GDP.M$mean)), 
                        gdp = fc.var$forecast$GDP.M$mean,
                        gdp.lo = fc.var$forecast$GDP.M[["lower"]][,2],
                        gdp.hi = fc.var$forecast$GDP.M[["upper"]][,2],
                        cpi = fc.var$forecast$CPI$mean,
                        cpi.lo = fc.var$forecast$CPI[["lower"]][,2],
                        cpi.hi = fc.var$forecast$CPI[["upper"]][,2],
                        unemp = unemp.fc[358:393],
                        unemp.lo = unemp.lo.fc[358:393],
                        unemp.hi = unemp.hi.fc[358:393],
                        exomat.path)


write_xlsx(var_ouput, "C:/Users/James/My Drive/a.Classes/Fall 2022/ECON 485/R Forecasting/temp_wd/Output/var_ouput_usgdp_risk.xlsx")





























# quarterly component VAR
#####
# VAR Quarterly GDP Components

# make monthly variables quarterly
monthly <- ts(cpi, start = c(1993, 1), frequency = 12)
cpi.q <- aggregate(monthly, nfrequency = 4)

monthly <- ts(unemp.rt, start = c(1993, 1), frequency = 12)
unemp.q <- aggregate(monthly, nfrequency = 4)/3

monthly <- ts(us.gdp, start = c(1992, 1), frequency = 12)
us.gdp.q <- aggregate(monthly, nfrequency = 4)

TRGT <- aggregate(TRGT, nfrequency = 4)/3

GSCPI.Q <- aggregate(gscpi, nfrequency = 4)/3

# stationary
CPI.Q <- diff(log(cpi.q), 4)
GDP.Q <- diff(log(gdp.q), 4)
#UNEMP.Q <- diff(log(unemp.q), 4)
UNEMP.Q <- unemp.q

US.GDP.Q <- diff(log(us.gdp.q), 4)

# prepare for estimation

data3 <- cbind(CPI.Q, GDP.Q, UNEMP.Q)


date.est.start <- c(1998, 1)
date.est.end <- c(2022,2)

# exogenous matrix
exomat <- cbind(TRGT, GSCPI.Q, US.GDP.Q)
tail(exomat)
head(exomat)

# exomat dates
exomat <- window(exomat, start = date.est.start, end = date.est.end)
colnames(exomat) <- c('TRGT', 'X.GSCPI', 'X.USGDP')

# data4 window dates
date.est.end <- c(2022, 2)
data4 <- window(data3, start = date.est.start, end = date.est.end)

tail(data4)

# estimate model

VARselect(data4, lag.max = 3, type = "const", exogen = exomat)

n.lag <- 1

# model
mod.var <- VAR(data4,  p = n.lag, type = c("const"), exogen = exomat)

# Observe coefficients and roots
coef(mod.var)
roots(mod.var)

# check Granger Causality
causality(mod.var, cause = 'CPI.Q') 
causality(mod.var, cause = 'GDP.Q') 
causality(mod.var, cause = 'UNEMP.Q') 

# paths
X.TRGT.path <- aggregate(newbase.ts, nfrequency = 4)/3
X.USGDP.path <- aggregate(base_recession_monthly, nfrequency = 4)/3
X.GSCPI <- aggregate(base.gscpi, nfrequency = 4)/3

# path matrix
exomat.path <- cbind(X.TRGT.path, X.GSCPI, X.USGDP.path)
colnames(exomat.path) <- c('TRGT', 'X.GSCPI', 'X.USGDP')

nrow(exomat.path)
# forecast
h <- 12
fc.gdp <- forecast(mod.var, h = h, dumvar = exomat.path)
plot(fc.gdp, include = 12)

fc.var$model$varresult

# consumption 
# make stationary
CON <- diff(log(con), 4)
data.con <- window(cbind(CON, data4[ , 1], data4[ , 3]), start = date.est.start, end = date.est.end)
colnames(data.con) <- c("Consumption", "CPI", "UNEMP")
VARselect(data.con, lag.max = 3, type = "const", exogen = exomat)

n.lag <- 1

# model
mod.var <- VAR(data.con,  p = n.lag, type = c("const"), exogen = exomat)

fc.con <- forecast(mod.var, h = h, dumvar = exomat.path)
plot(fc.con, include = 36)

# government
GOV <- diff(log(gov), 4)
data.gov <- window(cbind(GOV, data4[ , 1], data4[ , 3]),start = date.est.start, end = date.est.end)
colnames(data.gov) <- c("Government", "CPI", "UNEMP")
VARselect(data.gov, lag.max = 3, type = "const", exogen = exomat)

n.lag <- 1

# model
mod.var <- VAR(data.gov,  p = n.lag, type = c("const"), exogen = exomat)

fc.gov <- forecast(mod.var, h = h, dumvar = exomat.path)
plot(fc.gov, include = 36)

# investment
INV <- diff(log(inv), 4)
data.inv <- window(cbind(INV, data4[ , 1], data4[ , 3]),start = date.est.start, end = date.est.end)
colnames(data.inv) <- c("Investment", "CPI", "UNEMP")
VARselect(data.inv, lag.max = 3, type = "const", exogen = exomat)

n.lag <- 1

# model
mod.var <- VAR(data.inv,  p = n.lag, type = c("const"), exogen = exomat)

fc.inv <- forecast(mod.var, h = h, dumvar = exomat.path)
plot(fc.inv, include = 36)

# exports
XPRTS <- diff(log(xprts), 4)
data.XPRTS <- window(cbind(XPRTS, data4[ , 1], data4[ , 3]),start = date.est.start, end = date.est.end)
colnames(data.XPRTS) <- c("Exports", "CPI", "UNEMP")
VARselect(data.XPRTS, lag.max = 3, type = "const", exogen = exomat)

n.lag <- 1

# model
mod.var <- VAR(data.XPRTS,  p = n.lag, type = c("const"), exogen = exomat)

fc.XPRTS <- forecast(mod.var, h = h, dumvar = exomat.path)
plot(fc.XPRTS, include = 36)

# imports
MPRTS <- diff(log(mprts), 4)
data.MPRTS <- window(cbind(MPRTS, data4[ , 1], data4[ , 3]),start = date.est.start, end = date.est.end)
colnames(data.MPRTS) <- c("Imports", "CPI", "UNEMP")
VARselect(data.MPRTS, lag.max = 3, type = "const", exogen = exomat)

n.lag <- 1

# model
mod.var <- VAR(data.MPRTS,  p = n.lag, type = c("const"), exogen = exomat)

fc.MPRTS <- forecast(mod.var, h = h, dumvar = exomat.path)
plot(fc.MPRTS, include = 36)

NEX <- fc.XPRTS$forecast$Exports$mean - fc.MPRTS$forecast$Imports$mean

plot(NEX)
plot(fc.XPRTS$forecast$Exports$mean)
plot(fc.MPRTS$forecast$Imports$mean)


# export dataframe

df_export_components <- data.frame(date = as.Date(time(fc.gdp$forecast$GDP.Q$mean)), 
                                   gdp = fc.gdp$forecast$GDP.Q$mean, 
                                   consumption = fc.con$forecast$Consumption$mean,
                                   investment = fc.inv$forecast$Investment$mean, 
                                   government = fc.gov$forecast$Government$mean,
                                   net_exports = NEX)


write_xlsx(df_export_components, "C:/Users/James/My Drive/a.Classes/Fall 2022/ECON 485/R Forecasting/temp_wd/Output/basecompcast.xlsx")

investment <- data.frame(date = as.Date(time(INV)), 
                                   Investment = INV)
write_xlsx(investment, "C:/Users/James/My Drive/a.Classes/Fall 2022/ECON 485/R Forecasting/temp_wd/Output/basecompcast.xlsx")
