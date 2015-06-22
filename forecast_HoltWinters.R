# Fonte: http://www.quantlego.com/howto/holt-winters-smoothing-and-forecast/
# Simple Exponential Smoothing #####

# If you have a time series that can be described using an additive model with constant level and no 
# seasonaility, you can use simple exponential smoothing to make short-term forecast.

rain<-scan("http://library.quantlego.com/Howto/R/precip1.dat", skip=1)
rainseries<-ts(rain,start=c(1813))
plot(rainseries)


#As can be seen, there is roughly constant (mean) level of 25 inches in rainfall, and the random 
# fluctuation seems to be roughly constant in size over time. Therefore, it is probably appropriate 
# to describe the data using an additive model, and we can make forcasts using simple exponential smoothing.

# To use R's HoltWinters() function for simple exponential smoothing, we need to set the parameters beta=FALSE 
# and gamma=FALSE.

rainseriesforecasts<-HoltWinters(rainseries, beta=FALSE, gamma=FALSE) #Estimação dos parametros do modelo
rainseriesforecasts

rainseriesforecasts$fitted

plot(rainseriesforecasts)

rainseriesforecasts$SSE #Medida de acurácia

layout(1:4)
plot(HoltWinters(rainseries, alpha=0.25, beta=FALSE, gamma=FALSE), main="Alpha=0.25")
plot(HoltWinters(rainseries, alpha=0.5, beta=FALSE, gamma=FALSE), main="Alpha=0.5")
plot(HoltWinters(rainseries, alpha=0.75, beta=FALSE, gamma=FALSE), main="Alpha=0.75")
plot(HoltWinters(rainseries, alpha=1, beta=FALSE, gamma=FALSE), main="Alpha=1")

plot(HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=35)) # neste caso dizemos que a série estimada deve começar em 35

#install.packages("forecast")
require(forecast)
rainseriesforecasts2<-forecast.HoltWinters(rainseriesforecasts, h=8) # Previsão para os próximos 8 anos
rainseriesforecasts2
mean(rainseries)
plot.forecast(rainseriesforecasts2)

# Note, the 80% and 95% prediction intervals shown here are based on assumptions that there are no autocorrelations in the 
# forecast errors, and the forecast errors are normally distributed with mean zero and constant variance.

rainseriesforecasts2$residuals

acf(rainseriesforecasts2$residuals, lag.max=20)

# As can be seen, the correlation at lag 3 is just touching the significance bounds. To test whether there is significant 
# evidence of non-zero correlation at lags 1~20, we can carry out a Ljung-Box test using R's Box.test() function.

Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")

# Here the Ljung-Box test statistic is 17.4, and the p-value is 0.6, so there is little evidence of non-zero 
# autocorrelations in the in-sample forecasts errors at lags 1~20.

plot(rainseriesforecasts2$residuals)

#  R function saved as 'Plot_Forecast_Errors_Histogram.R'
PlotForecastErrors <- function(forecasterrors){
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors)-mysd*5
  mymax <- max(forecasterrors)+mysd*5
  mynorm <- rnorm(10000,mean=0,sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2<mymin)
  {
    mymin<-mymin2
  }
  if (mymax2>mymax)
  {
    mymax<-mymax2
  }
  mybins <- seq(mymin,mymax,mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


#source("c:/users/MyName/desktop/Plot_Forecast_Errors_Histogram.R")
PlotForecastErrors(rainseriesforecasts2$residuals)

# The histogram shows that the distribution of forecast errors is roughly 
# centered on zero, and is more or leass normally distributed, although it 
# seems to slightly skewed to the right compared to a normal curve. However, 
# the right skew is relatively small, and so it is plausible that the forecast 
# errors are normally distributed with mean zero.

# In summary: The Ljung-Box test showed that there is little evidence of non-zero 
# autocorrelations in the in-sample forecast errors, and the distribution of forecast
# errors seems to be normally distributed with mean zero. This suggests that the simple 
# exponential smoothing method provides an adequate predictive model for the rainfall, 
# which probably cannot be improved upon. Furthermore, the assumption that the 80% abd 95% 
# prediction intervals were based on are probably valid.


# Holt's Exponential Smoothing #####

# If you have a time series that can be described using an additive model with increasing or 
# decreasing trend and no seasonality, you can use Holt's exponential smoothing to make 
# short-term forecasts.


www <- "http://library.quantlego.com/Howto/R/skirts.dat"
skirts <- scan(www, skip=5)
skirts.ts <- ts(skirts,start=c(1866))
plot(skirts.ts)


skirts.ts.forecasts <- HoltWinters(skirts.ts, gamma=FALSE)
skirts.ts.forecasts

# Note that the estimated values of \(\alpha=0.84\,\) and \(\beta=1\,\) are both high, 
# meaning that both the estimate of the current value of the level, and of the slope of the 
# trend component, are based mostly on very recent observations in the time series.

plot(skirts.ts.forecasts)

skirts.ts.forecast2 <- forecast.HoltWinters(skirts.ts.forecasts, h=19)  # Previsão para 19 anos
skirts.ts.forecast2

plot(skirts.ts.forecast2)

acf(skirts.ts.forecast2$residuals, lag.max=20)
Box.test(skirts.ts.forecast2$residuals, lag=20, type="Ljung-Box")


# The correlogram shows that the sample autocorrelation for the in-sample forecast errors at lag 5 
# exceeds the significance bounds. However, we would expect one in 20 of the autocorrelations for the 
# first 20 lags to exceed the 95% significance bounds by chance alone. The p-value from the Ljung-Box 
# test is 0.47, indicating that there is little evidence of non-zero autocorrelation in the in-sample 
# forecast errors at lags 1~20.


layout(1:2)
plot(skirts.ts.forecast2$residuals)
source("c:/MyRCodes/Plot_Forecast_Errors_Histogram.R")
PlotForecastErrors(skirts.ts.forecast2$residuals)

# As can be seen, the forecast errors have roughly constant variance over time. The histogram shows 
# that it is plausible that the forecast errors are normally distributed with mean zero and constant variance.

# In summary: The ljung-Box test shows that there is little evidence of autocorrelations in the forecast errors, 
# while the time plot and histogram of forecast errors show that it is plausible that the forecast errors are normally 
# distributed with zero mean and constant variance. Therefore, we can comclude that Holt's exponential smoothing 
# provides an adequate predictive model which probably cannot be improved upon. In addition, it means that the 
# assumptions that the 80% and 95% prediction intervals were based on are probably valid.


# Holt-Winters Exponential Smoothing  #####

# If you have a time series rhat can be described using an additive model with increasing or decreasing 
# and seasonality, then you can use Holt-Winters exponential smoothing to make sjort-term forecast.

souvenir <- scan("http://library.quantlego.com/Howto/R/fancy.dat")
souvenir.ts <- ts(souvenir, start=c(1987,1), freq=12)
log.souvenir.ts <- log(souvenir.ts)
layout(1:2)
plot(log.souvenir.ts)
plot(souvenir.ts)

souvenir.forecast <- HoltWinters(log.souvenir.ts)
souvenir.forecast
souvenir.forecast$SSE

# The \(\alpha=0.41\,\)  is relatively low, indicating that the estimate of the level at the current time 
# point is based upon both recent observations and some observations in the more distant past.
# The \(\beta=0\,\) indicates that the estimate of the slope of the trend component is not updated over the 
# time series. Instead is set equal to its initial value. This makes good intuitive sense, as the level changes 
# quite a bit over the time series, but the slope of the trend component remains roughly the same.
# The \(\gamma=0.96\,\) is high, indicating that the estimate of the seasonal component at the current time point
# is just based on very recent observations.

plot(souvenir.forecast)

souvenir.forecast2 <- forecast.HoltWinters(souvenir.forecast, h=48)
souvenir.forecast2
plot(souvenir.forecast2)

acf(souvenir.forecast2$residuals, lag.max=20)
Box.test(souvenir.forecast2$residuals, lag=20, type="Ljung-Box")

layout(1:2)
plot(souvenir.forecast2$residuals)
PlotForecastErrors(souvenir.forecast2$residuals)

# As can be seen, it is plausible that the forecast errors have constant variance over time, and they 
# are normally distributed with mean zero.

# In summary: There is little evidence of autocorrelation at lags 1~20 for the forecast errors, and 
# the forecast errors appear to be normally distributed with mean zero and constant variance over time. 
# This suggest that Holt-Winters exponential smoothing provides an adequate predictive model which probably 
# cannot be improved uopn. FUrthermore, the assumptions upon which the prediction intervals were based are 
# probably valid.


# Another Example ####

www <- "http://library.quantlego.com/Howto/R/wine.dat"
wine.dat <- read.table(www, header = T)
sweetw.ts <- ts(wine.dat$sweetw, start=c(1980,1), freq=12)
sweetw.hw <- HoltWinters(sweetw.ts, seasonal="mult")
sweetw.hw

# Here \(\alpha=0.41,\,\beta=0,\,\gamma=0.47\), meaning that the level and seasonal variation adapt rapidly 
# whereas the trend is slow to do so.

# The coefficients are the estimated values of the level, slope, and the 12 multiplicative seasonals from 
# January to December available at the latest time point.

# To see the predictive performance of the model, we can compare the mean square one-step-ahead prediction 
# error with the standard deviation of the original time series.

sqrt(sweetw.hw$SSE/length(sweetw.ts))
sd(sweetw.ts)

# As can be seen, we see a substantial decrease from 50.5 to 121.39.
# By plotting the fotted component of the HW smoothing result, we can get the decomposed plots

sweetw.hw$fitted
plot(sweetw.hw$fitted)

library(forecast)
sweetw.hw2 <- forecast.HoltWinters(sweetw.hw, h=4*12)
plot(sweetw.hw2)


# Last Example ####

AP <- AirPassengers
AP.hw <- HoltWinters(AP, seasonal="mult")
plot(AP.hw)
plot(AP.hw$fitted)
AP.predict <- forecast.HoltWinters(AP.hw, h = 4*12)
plot(AP.predict)


