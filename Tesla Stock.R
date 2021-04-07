#importing the packages
library(quantmod)
library(forecast)
library(tseries)
library(timeSeries)
library(xts)
library(caTools)
install.packages("timeSeries")


#loading data
data<- read.csv("C:\\Users\\19736\\Desktop\\Predictive Analytics\\Time series Forecasting\\TSLA.csv")
data
attach(data)

#want the close price
tesla=data[,5]
par(mfrow=c(1,1))
plot(tesla)

#get the arima p,d,q values
Acf(tesla, main='ACF for differenced series')
Pacf(tesla, main='PACF for differenced series')
auto.arima(tesla,seasonal=FALSE) #pdq is 5,2,0

#log residuals for the stock
logs=diff(log(tesla),lag=1)
logs=logs[!is.na(logs)]

#plot the log returnsfor more accurate forecasting
par(mfrow=c(1,1))
plot(logs,type='l',main ='log returns plot')

#Augumented Dickey-Fuller Test(ADF) for p-value
print(adf.test(logs))

auto.arima(logs,seasonal = FALSE)
str(logs)

#Split the data into train and test 80/20
set.seed(109)
#train_indices <- sample.int(n=nrow(logs),size = floor(0.80*nrow(logs)),replace = FALSE)
sample=sample.split(logs,SplitRatio=0.80)
train = subset(logs,sample==TRUE)
test = subset(logs,sample==FALSE)

par(mfrow=c(1,2))
Acf(train, main="ACF for Differenced Series")
Pacf(train, main="PACF for Differenced Series")
auto.arima(train,seasonal=FALSE)

#plotting the models, getting the accuracy and conclusions
fit1 <- auto.arima(train,seasonal=FALSE)
tsdisplay(residuals(fit1),lag.max = 40,main = '(0,0,0) Model Residuals')

#custom arima
fit2 <- arima(train,order = c(5,0,13))
tsdisplay(residuals(fit2),lag.max = 40,main = '(5,0,13) Model Residuals')

#Original Data
fit3 = auto.arima(tesla,seasonal=FALSE)
tsdisplay(residuals(fit3),lag.max = 40,main = 'Original Non-log Residuals')


fit4 = arima(tesla,order = c(5,0,13))
tsdisplay(residuals(fit4),lag.max = 40,main = '(5,0,13) Model Residuals on original dataset')

#plotting the arima models
par(mfrow=c(2,2))
Period<-100
fcast1<-forecast(fit1,h=Period)
plot(fcast1)
fcast2<-forecast(fit2,h=Period)
plot(fcast2)
fcast3<-forecast(fit3,h=Period)
plot(fcast3)

par(mfrow=c(1,2))
plot(fcast3)

#see the accuracy of the models
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)
    
fcast3









