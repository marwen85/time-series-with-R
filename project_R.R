library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(timeDate)
library(prophet)
library(tidyverse)
getSymbols('META', from= '2014-01-01', to= '2023-12-1')

meta_df <- META[,4]
meta_df
plot(meta_df)


par(mfrow = c(1,2))
acf(meta_df, main='acf for the serie')
pacf(meta_df, main = 'pacf for the serie')

auto.arima(meta_df, seasonal = FALSE)

log_meta =diff(log(meta_df))
plot(log_meta)
par(mfrow = c(1,2))
acf(log_meta, main='acf for the serie')
pacf(log_meta, main = 'pacf for the serie')


sample_size = floor(0.80 * nrow(log_meta))
set.seed(109)
train_indices = sample(seq_len(nrow(log_meta)) , size = sample_size)

train <- log_meta[train_indices]
test <-  log_meta[-train_indices]

par(mfrow = c(1,2))
acf(train, main='acf for the serie')
pacf(train, main = 'pacf for the serie')


fitl = auto.arima(train, seasonal = FALSE)

tsdisplay(residuals(fitl), lag.max=10)

fitl2= arima(train, order = c(8,1,8))
tsdisplay(residuals(fitl), lag.max=10)
fitl2

fitl3 = arima(train, order = c(6,1,8))
tsdisplay(residuals(fitl), lag.max=10)
fitl3


fitl4= arima(train, order = c(3,1,8))
tsdisplay(residuals(fitl), lag.max=10)


par(mfrow= c(2,2))
period = 20
fcast= forecast(fitl, h=period)
plot(fcast)


fcast1= forecast(fitl2, h=period)
plot(fcast1)


fcast2= forecast(fitl3, h=period)
plot(fcast)



fcast3= forecast(fitl4, h=period)
plot(fcast)


accuracy(fcast)
accuracy(fcast1)
accuracy(fcast2)
accuracy(fcast3)