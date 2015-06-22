###############################
## Modelo Autoregressivo
###############################

# Ativando o módulo de séries temporais
library(tseries)


serie<-read.ts("telesp.txt", header = FALSE, sep = "", skip = 0)

# Gráfico da serie

ts.plot(serie)

# teste para verificar se a serie e estacionaria

adf.test(serie)

# Gráfico do ACF e PACF

par(mfrow=c(1,2))
acf(serie)
pacf(serie)



# AR com média diferente de zero

arima(serie, order = c(2,0,0) )


# AR com média ZERO

arima(serie, order = c(2,0,0) , include.mean=FALSE)


# Análise dos res�???duos do modelo

modelo <- arima(serie, order = c(2,0,0) , include.mean=FALSE)

par(mfrow=c(1,2))
acf(residuals(modelo))
pacf(residuals(modelo))


# AR Incompleto

# AR com média zero

modelo<-arma(serie, order = c(1,1), lag = list(ar=c(5,14,20)),include.intercept = FALSE)
modelo
modelo$asy.se.coef

for(i in 1:length(modelo$coef)) print(modelo$coef[i]/modelo$asy.se.coef[i])

# Análise dos res�???duos do modelo

res<- modelo$res

n <- length(serie)

par(mfrow=c(1,2))
acf(res[21:n])
pacf(res[21:n])




# AR com média diferente de zero

modelo<-arma(serie, order = c(1,1), lag = list(ar=c(5,14)))
modelo
modelo$asy.se.coef

for(i in 1:length(modelo$coef)) print(modelo$coef[i]/modelo$asy.se.coef[i])

# Análise dos res�???duos do modelo

res<- modelo$res

n <- length(serie)

par(mfrow=c(1,2))
acf(res[21:n])
pacf(res[21:n])


#############################
## Media Movel 
############################## 



# Importando o arquivo com a série temporal para o R

# Salvar a série temporal no Excel com o formato texto(separado por tabulações) 

serie<-read.ts("serie23.txt", header = FALSE, sep = "", skip = 0)

# Gráfico da serie

ts.plot(serie)

# teste para verificar se a serie e estacionaria

adf.test(serie)


# Gráfico do ACF e PACF

par(mfrow=c(1,2))
acf(serie)
pacf(serie)

# Modelo ARMA com intercepto

modelo <- arima(serie, order = c(0,0,2))
acf(residuals(modelo))
pacf(residuals(modelo))

# Modelo ARMA sem intercepto

modelo <- arima(serie, order = c(0,0,2), include.mean=FALSE)
acf(residuals(modelo))
pacf(residuals(modelo))

# Análise dos res�???duos do modelo

modelo <- arima(serie, order = c(0,0,1), include.mean=FALSE)
acf(residuals(modelo))
pacf(residuals(modelo))

############################################
# Como determinar os par�metros de um ARIMA
############################################

#Fonte: http://stats.stackexchange.com/questions/104818/determining-parameters-p-d-q-for-arima-modeling

require(forecast)

sales <- ts(c(99, 58, 52, 83, 94, 73, 97, 83, 86, 63, 77, 70, 87, 84, 60, 105, 87, 
              93, 110, 71, 158, 52, 33, 68, 82, 88, 84),frequency=12)

plot(sales)

acf(sales)
pacf(sales)

model <- auto.arima(sales)
model

plot(forecast(model))


# Two things.Your time series is monthly,you need at least 4 years of data for a sensible ARIMA estimation,
# as reflected 27 points do not give the autocorrelation structure. This can also mean that your sales is 
# affected by some external factors , rather than being correlated with its own value. Try to find out what 
# factor affects your sales and is that factor being measured. Then you can run a regression or 
# VAR (Vector Autoregression) to get forecasts.

install.packages("tsoutliers")
require(tsoutliers)

fit1 <- auto.arima(sales, d=0, D=0, ic="bic")
fit1

JarqueBera.test(residuals(fit1))[[1]]
#  notice that the null of normality in the residuals is rejected at the 5% significance level.

res <- tsoutliers::tso(sales, types=c("AO", "TC", "LS"), 
                       args.tsmethod=list(ic="bic", d=0, D=0))
res

JarqueBera.test(residuals(res$fit))[[1]]
