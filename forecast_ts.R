Fonte: http://www.statmethods.net/advstats/timeseries.html

install.packages("XLConnect")
require("XLConnect")
setwd("C:\\Users\\Euler\\Documents\\GitHub\\time series")

#Fonte para importar dados
# http://www.milanor.net/blog/?p=779
#myvector <- loadWorkbook("ipca_201504SerieHist.xls")
mv <- readWorksheetFromFile("ipca_201504SerieHist.xlsx",
                                  sheet=2)
#mv <- mv[,4]
#leng_mv <- nrow(mv)

myvector <- mv[1:48,4]

myts <- ts(myvector, start=c(2009,1),end=c(2014,12),
           frequency=12)

myts
plot(myts)


# Primeiro estudo ####
#Seasonal Decomposition

fit <- stl(myts, s.window="period")
plot(fit)

# additional plots
monthplot(myts)
install.packages("forecast")
library(forecast)
seasonplot(myts)


# simple exponential - models level
fit <- HoltWinters(myts, beta=FALSE, gamma=FALSE)

# double exponential - models level and trend
fit <- HoltWinters(myts, gamma=FALSE)

# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(myts)

# predictive accuracy
library(forecast)
accuracy(fit)

# predict next three future values
library(forecast)
forecast(fit, 3)
plot(forecast(fit, 3))


# fit an ARIMA model of order P, D, Q
p=1
q=1
d=0
fit <- arima(myts, order=c(p, d, q))
             
# predictive accuracy
library(forecast)
accuracy(fit)

# predict next 5 observations
library(forecast)
forecast(fit, 5)
plot(forecast(fit, 5))


library(forecast)
# Automated forecasting using an exponential model
fit <- ets(myts)

# Automated forecasting using an ARIMA model
fit <- auto.arima(myts)

# Segundo estudo


