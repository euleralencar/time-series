# Morretin examples

getwd()
setwd("C:\\Users\\Euler\\Documents\\GitHub\\time series\\dados_moretin")

#install.packages("xlsx")
require("xlsx")

#Cap. 1
#Exercício 2
ICV <- read.xlsx("ICV.xls",sheetName="Plan1")

#head(ICV)
#tail(ICV)

ICV.ts <- ts(ICV[,2],start=c(1970,1),end=c(1980,6),frequency=12)

plot(ICV.ts)

ICV.1dif <- diff(ICV.ts,differences=1)

plot(ICV.1dif)

ICV.2dif <- diff(ICV.ts,differences=2)

plot(ICV.2dif)

# Exercício 3

Temp <- read.xlsx("temperatura.xls",sheetName="Plan1")
head(Temp)
tail(Temp)
Temp.ts <- ts(Temp[,-1],start=c(1976,1),end=c(1985,12),
              frequency=12)

plot(Temp.ts)

Temp.1dif <- diff(Temp.ts,differences=1)

plot(Temp.1dif)

Temp.2dif <- diff(Temp.ts,differences=2)

plot(Temp.2dif)

# Exercício 4

Energ <- read.xlsx("ENERGIA.xls",sheetName="Plan1")
head(Energ)

Energ.ts <- ts(Energ[,3], start=c(1968,1), end=c(1979,9),frequency=12)
plot(Energ.ts)

Energ.1dif <- diff(Energ.ts,differences=1)
plot(Energ.1dif)
Energ.2dif <- diff(Energ.ts,differences=2)
plot(Energ.2dif)


#aplicando tranformacao logaritimca

Energ.log <- log(Energ.ts)
plot(Energ.log)

Energ.log.1dif <- diff(Energ.log,differences=1)
plot(Energ.log.1dif)


# Questao 7

Banespa <- read.table("D-BANESPA.txt",header=F, quote="\"",dec=".")
#head(Banespa)

#load(url("http://www.ime.usp.br/~pam/D-BANESPA.Rdata"))
#rm(Banespa1,Banespaa)

Banespa.ts <- ts(Banespa,frequency=252)
            
summary(Banespa.ts)

plot(Banespa.ts)

#study about function scan and quote inside

#install.packages("moments")
#require(moments)

estat <- function(x){
      col1 <- c("min","max","média","mediana","desvpad","quantil25","quantil75","assimetria","curtose")
      col2 <- round(c(min(x), max(x), mean(x),  median(x),  sd(x),  quantile(x)[2] ,  quantile(x)[4] ,skewness(x), kurtosis(x)),4)
      cbind(col1,col2)
}


estat(Banespa.ts)
estat(diff(log(Banespa.ts)))
#Caso queira substituir faltantes pela média
#is.na(x) = mean(x, na.rm = TRUE)

hist(Banespa.ts , freq=F)
rug(Banespa.ts)
lines(density(Banespa.ts),col="red")

gernorm <- rnorm(1500,mean=mean(Banespa.ts),
                 sd=sd(Banespa.ts))
lines(density(gernorm),col="blue")


#logrban <- log(Banespa.ts)

logret <- diff(log(Banespa.ts))
cutpoints <- seq( 1 , length(logret) , by = 252 )
categories <- findInterval( 1:length(logret) , cutpoints )
retorno_anual <- tapply( logret , categories , sum )
mean(retorno_anual)*100

#retorno após 5 anos
taxaretorno <- sum(logret)
taxaretorno <- log(Banespa.ts[1499])-log(Banespa.ts[1])
retorno <- taxaretorno*10000

# Capitulo 3
require(xlsx)
Energ <- read.xlsx("ENERGIA.xls",sheetName="Plan1")

Energ.ts <- ts(Energ[,3], start=c(1968,1), end=c(1979,9),frequency=12)
plot(Energ.ts)

require(stats)
Energ1 <- window(Energ.ts, start=c(1977,1), end=c(1978,12))
Energ2 <- Energ1/1000

t <- seq(1:24)

tendencia <- lm(Energ2 ~ t)
summary(tendencia)
plot(Energ2)

abline(tendencia)

require(forecast)
nd <- seq(25,28)
fcast <- forecast(tendencia, h=4, level=c(80,95), newdata=nd)
#predict(tendencia)
plot(fcast)


#Resultados obtidos com o livro
teste <- function(x){
  z <- 68.438+4.242*x
  z
}

teste(nd)
