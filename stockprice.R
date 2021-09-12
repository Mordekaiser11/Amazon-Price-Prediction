
library(prophet)
library(quantmod)
library(forecast)
library("xlsx")
library(tseries)
library(timeSeries)
library(dplyr)
library(fGarch)

getSymbols("AMZN", src="yahoo", from="2015-01-01",to='2019-09-09')

class(AMZN)

AMZN_Close = AMZN[,4]

plot(AMZN_Close)

class(AMZN_Close)

par(mfrow=c(1,1))
Acf(AMZN_Close,main="ACF For Differenced Series")
Pacf(AMZN_Close,main="PACF For Differenced Series")

print(adf.test(AMZN_Close))

auto.arima(AMZN_Close,seasonal=FALSE)

fitedilenA = auto.arima(AMZN_Close,seasonal=FALSE)
tsdisplay(residuals(fitedilenA),lag.max = 30,main = '(3.1.4) Model Residuals')

auto.arima(AMZN_Close,seasonal=FALSE)


fitedilenB = auto.arima(AMZN_Close,order= c(1,2.4))

tsdisplay(residuals(fitedilenB),lag.max = 30,main = '(1.2.4) Model Residuals')

fitedilenC = auto.arima(AMZN_Close,order=c(5,1,4))

tsdisplay(residuals(fitedilenB),lag.max = 30,main = '(5.1.4) Model Residuals')

fitedilenD = auto.arima(AMZN_Close,order=c(1,1,1))
auto.arima(AMZN_Close,seasonal=FALSE)

tsdisplay(residuals(fitedilenB),lag.max = 30,main = '(1.1.1) Model Residuals')


par(mfrow=c(2,2))

sayý<-100

tahmin1<-forecast(fitedilenA,h=sayý)

plot(tahmin1)

tahmin2<-forecast(fitedilenB,h=sayý)

plot(tahmin2)


tahmin<-forecast(fitedilenB,h=sayý)

plot(tahmin)

tahmin4<-forecast(fitedilenD,h=sayý)

plot(tahmin4)


accuracy(tahmin1)
accuracy(tahmin2)
accuracy(tahmin)
accuracy(tahmin4)



head(tahmin1$upper)

head(tahmin1$lower)

head(tahmin2$upper)
head(tahmin2$lower)

head(tahmin$upper)
head(tahmin$lower)

head(tahmin4$upper)
head(tahmin4$lower)





# BURAYA KADAR BAKARAK
# BURASI BAÞKA BÝÞEY

modelfit <- auto.arima(close_price, lambda = "auto")

Box.test(modelfit$residuals, lag= 2, type="Ljung-Box")

Box.test(modelfit$residuals, type="Ljung-Box")


price_forecast <- forecast(modelfit, h=30)

plot(price_forecast)

head(price_forecast$mean)


head(price_forecast$lower)

head(price_forecast$upper)


N = length(close_price)
n = 0.7*N
train = close_price[1:n, ]
test  = close_price[(n+1):N,  ]
trainarimafit <- auto.arima(train, lambda = "auto")
predlen=length(test)
trainarimafit <- forecast(trainarimafit, h=predlen)


