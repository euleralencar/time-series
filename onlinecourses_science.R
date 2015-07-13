# Fonte: https://onlinecourses.science.psu.edu/stat510/?q=node/70

#oilindex <- arima.sim(list(order=c(1,1,1),ar=0.9, ma=-0.2),n=100)

#oilindex = scan("oildata.dat")
#write.csv(oilindex,"C:\\Users\\Euler\\Documents\\GitHub\\time series\\oil.txt", quote=)

oilindex <-read.csv("C:\\Users\\Euler\\Documents\\GitHub\\time series\\oil.txt")
oilindex <- oilindex[,2]
plot (oilindex, type = "p", main = "Log of Oil Index Series")
expsmoothfit = arima (oilindex, order = c(0,1,1))
expsmoothfit # to see the arima results
predicteds = oilindex - expsmoothfit$residuals # predicted values
plot(predicteds)
plot (oilindex, type="b", main = "Exponential Smoothing of Log of Oil Index")
lines (predicteds)
1.5139*oilindex[100]-0.5139*predicteds[100] # forecast for time 101

length(oilindex)

oilindex[100]


# Smoothing with SMA
install.packages("TTR")
require(TTR)


kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))


#ajustando um média móvel simples
plot.ts(kingstimeseries)
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
lines(kingstimeseriesSMA3,col=2)
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
lines(kingstimeseriesSMA8,col=4)


plot.ts(birthstimeseries)
birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)

#Retirando efeito sazonal da série
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseries)
lines(birthstimeseriesseasonallyadjusted,col=2)

birthstimeseriesseasonallyadjusted2 <- birthstimeseries - birthstimeseriescomponents$seasonal - birthstimeseriescomponents$trend
plot(birthstimeseries)
plot(birthstimeseriesseasonallyadjusted2,col=2)

# Forecast Simple Exponential Smoothing

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot(rainseries)

rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
plot(rainseriesforecasts)
HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)


library("forecast")

rainseriesforecasts2 <- forecast.HoltWinters(rainseriesforecasts, h=8)
plot.forecast(rainseriesforecasts2)

#Verficar se existe algum correlação nos resíduos
acf(rainseriesforecasts2$residuals, lag.max=20)

# To test whether there is significant evidence for non-zero correlations at lags 1-20, we can carry out a Ljung-Box test. 
# H0: não há correlação nos erros
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")

plot.ts(rainseriesforecasts2$residuals)



