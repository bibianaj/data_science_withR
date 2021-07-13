###############################################################################
# Tims Series Analaysis and Forecasting with Avatar Daily Gross Profit data
# Created by : Jeong Eunbi (TP052184)
###############################################################################

library(forecast)
library(tseries)
library(pracma)
library(uroot)


# 0.0. Read File
setwd("C:/Users/Bibi/Desktop/Master Course/TSF/Assignment")
avatar_data = read.csv("Avatar.csv", header=TRUE)
#avatar_data
#plot(avatar_data$Date, avatar_data$Gross)


# 1.0. Generate Plot for Daily Gross Profit 
#----- 1st Date - 2009-12-18 / Last Date - 2010-11-18 

#as.numeric(as.Date("2009-12-18") - as.Date("2010-11-18"))
days <- seq(as.Date("2009-12-18"), as.Date("2010-11-18"), by = "day")
avatar_ts = ts(avatar_data$Gross, 
               start = c(2009, as.numeric(format(days[1], "%j"))), 
               frequency=365)
acf(avatar_ts)
pacf(avatar_ts)
tsdisplay(avatar_ts)
#avatar_ts
plot.ts(avatar_ts)
plot(avatar_ts, main="Avatar Daily Gross Profit", xlab="Date", ylab="Gross")


#d=as.Date(avatar_data$Date,format="%Y-%m-%d")
#plot.ts(d, avatar_data$Gross, main="Avatar Daily Gross Profit", xlab ="", ylab="Gross", xaxt="n", type="l")
#axis.Date(1, at = seq(d[1], d[length(d)], by = "month"), format = "%Y-%m-%d", las = 2)

# 3.0. Methododlogy Comparison 
##### 3.1. Forecast Techniques
#----- 1st Date - 2009-12-18 / Last Date - 2010-11-18 

##### Spliting Data as Train & Test
length(avatar_ts) # total length : 318
train = subset(avatar_ts, end=length(avatar_ts)*0.8)
test = subset(avatar_ts, start=length(avatar_ts)*0.8+1)

train = subset(avatar_ts, end=254)
test = subset(avatar_ts, start=255)

##### TS Display
tsdisplay(avatar_ts)
tsdisplay(train)
tsdisplay(test)
tsdisplay(diff(train, 1))
######### 3.1.1. Holt Method
# Alpha & Beta config test
alpha_beta_1 = holt(train, initial = "simple", alpha=0.6, beta=0.1)
alpha_beta_2 = holt(train, initial = "simple", alpha=1, beta=0.3)
alpha_beta_3 = holt(train, initial = "simple", alpha=0.6, beta=0.3)

accuracy(alpha_beta_1, test)
accuracy(alpha_beta_2, test)
accuracy(alpha_beta_3, test)

# No diff - MAPE 246.0151(Train) / 213.3497(Test)
plot(train, main="Avatar Daily Gross Profit", xlab="Date", ylab="Gross")

holt_method_1 = holt(train, initial = "simple", alpha=0.6, beta=0.1) #h - period (how many week)
lines(fitted(holt_method_1), col="red")
lines(fitted(naive(train)), col="blue", type ="o")

plot(holt_method_1)

holt_method_2 = holt(train, initial = "simple")
lines(fitted(holt_method_2), col ="blue")
accuracy(holt_method_1, test)
accuracy(holt_method_2, test)

summary(holt_method_2)

# alpha 1 & beta 0.3 : MAPE 64.34 / 617.64
# alpha 0.6 & beta 0.3 : MAPE 69.77 / 846.47
# alpha 0.6 & beta 0.1 : MAPE 59.97 / 544.86

######## 3.1.2. Exponential Trend Model 
plot(train, main="Avatar Daily Gross Profit", xlab="Date", ylab="Gross")

time = c(1:length(train))
#ET_method_season = tslm(log(train) ~ time + season, lambda = 0)
ET_method = tslm(log(train) ~ time)
#exp(coef(ET_method)[1])*exp(coef(ET_method)[2]*1)
#exp(fitted(ET_method))
summary(ET_method_season)

lines(exp(fitted(ET_method)), col="blue")
#lines(exp(fitted(ET_method_season)), col="red")

coef(ET_method)
summary(ET_method_season)
## To get MAPE result, trend model has to generate forecast model 
## MPAE 9.28/ 99.98
ET_forecast = forecast(ET_method, start(2010,07))
accuracy(ET_forecast, test)


#plot(ET_forecast, main="Avatar Daily Gross Profit", xlab="Date", ylab="Gross")

######## 3.1.3. Holts Winter Method
# HoltWinters  doesn't have any barrier from frequency
plot(avatar_ts, main="Avatar Daily Gross Profit", xlab="Date", ylab="Gross")

holt_winter_method = HoltWinters(train, seasonal = "multi", beta = FALSE, gamma = FALSE)
plot(holt_winter_method, main="Avatar Daily Gross Profit", xlab="Date", ylab="Gross")

## MAPE 48.766 / 211.021
HW_forecast = forecast(holt_winter_method, h=12) # Predict next 12 weeks
accuracy(HW_forecast, test)

plot(HW_forecast, main="Avatar Daily Gross Profit", xlab="Date", ylab="Gross")
accuracy(HWM_forecast,test)



##########################################
# Transformation - SQRT
##########################################
avatar_sqrt = sqrt(avatar_ts)
plot(avatar_sqrt)

train_sqrt = subset(avatar_sqrt, end=254)
test_sqrt = subset(avatar_sqrt, start=255)

# Holt MAPE 25.62 / 38.396
plot(train_sqrt,  main="Holt Method with SQRT Transformation", xlab="Date", ylab="Gross")
holt_method_sqrt = holt(train_sqrt, initial = "simple", alpha=0.6, beta=0.1)
lines(fitted(holt_method_sqrt), col="red")
accuracy(holt_method_sqrt, test_sqrt)

# Exponential Trend Model MAPE 9.28/96.90
plot(train_sqrt,  main="Exponential Trend Model with SQRT", xlab="Date", ylab="Gross")
time_sqrt = c(1:length(train_sqrt))
ET_method_sqrt = tslm(log(train_sqrt) ~ time_sqrt)
ET_forecast_sqrt = forecast(ET_method_sqrt, start(2010,07))
lines(exp(fitted(ET_method_sqrt)), col="red")
accuracy(ET_forecast_sqrt, test_sqrt)

# holt Winter MAPE 21.48 / 87.41
plot(train_sqrt,  main="Holt-Winters' with SQRT", xlab="Date", ylab="Gross")
holt_winter_sqrt = HoltWinters(train_sqrt, seasonal = "multi", beta = FALSE, gamma = FALSE)
HW_forecast_sqrt = forecast(holt_winter_sqrt, h=12) # Predict next 12 weeks
lines(fitted(HW_forecast_sqrt), col="red")
accuracy(HW_forecast_sqrt, test_sqrt)


##########################################
# Transformation - LOG (Best transformation)
##########################################
avatar_log = log(avatar_ts)

train_log = subset(avatar_log, end=254)
test_log = subset(avatar_log, start=255)

# Holt MAPE 4.22 / 4.78
plot(train_log, main="Holt Method with LOG")
holt_method_log = holt(train_log, initial = "simple", alpha=0.6, beta=0.1)
lines(fitted(holt_method_log), col="red")
accuracy(holt_method_log, test_log)

# Exponential Trend Model MAPE 3.64/74.72
plot(train_log, main="Exponential Trend Model with LOG")
time_log = c(1:length(train_log))
ET_method_log = tslm(log(train_log) ~ time_log)
lines(exp(fitted(ET_method_log)), col="red")
ET_forecast_log = forecast(ET_method_log, start(2010,07))
accuracy(ET_forecast_log, test_log)


# holt Winter MAPE 3.57/ 11.29
plot(train_log, main="Holt-Winters' with LOG")
holt_winter_log = HoltWinters(train_log, seasonal = "multi", beta = FALSE, gamma = FALSE)
HW_forecast_log = forecast(holt_winter_log, h=12) # Predict next 12 weeks
lines(fitted(HW_forecast_log), col="red")
accuracy(HW_forecast_log, test_log)


####################################
# Forecasting with Holt Method
####################################
avatar_ts_log = log(avatar_ts)
tsdisplay(avatar_ts_log)
holt_method_log = holt(avatar_ts_log, initial = "simple", alpha=0.6, beta=0.1)
holt_forecast_log = forecast(holt_method_log)
plot(holt_forecast_log, xlab="Date", ylab="Gross")

#####################################
# Check stationary or not
#####################################
adf.test(avatar_ts)

#####
# PRE-PROCESSING FOR ARIMA
# STEP 1 : Construct ACF&PACF with original ts 
# Step 2 : Perform seasonal differencing
####
library(lmtest)
library(astsa)
##### lambda = 0 -> log transform
##### lambda = 0.5 -> sqrt
##### lambda = 1 -> no transform
##### lambda = 2 -> square
##### lambda = -0.5 -> 1/squre root
##### lambda = -1 -> inverse 
##### lambda = -2 -> 1/square(y)
avatar_ts = ts(avatar_data$Gross, 
               start = c(2009, as.numeric(format(days[1], "%j"))), 
               frequency=7)

tsdisplay(avatar_ts)
acf2(avatar_ts)
avatar_ts_seasonal_diff = diff(avatar_ts, lag = 7, differences = 1)
tsdisplay(avatar_ts_seasonal_diff)
acf2(avatar_ts_seasonal_diff)

####################################
# SARIMA(1,0,0)(1,1,1)[7]
####################################
sarima_1 = Arima(avatar_ts_seasonal_diff, order=c(1,0,0), seasonal = c(1,1,1), lambda = 0) 
summary(sarima_1) # MAPE : 90.54
coeftest(sarima_1)
Box.test(sarima_1$residuals, lag=7)
sarima_1$aic  # Akaike Information Criteria // AIC : 210.8396

####################################
# SARIMA(1,0,0)(0,1,1)[7] : refer auto arima and applied seasaonal differencing 
####################################
sarima_2 = Arima(avatar_ts_seasonal_diff, order=c(1,0,0), seasonal = c(0,1,1), lambda = 0)
summary(sarima_2) # MAPE : 91.21052
coeftest(sarima_2)

####################################
# SARIMA modeling Result 
####################################
coeftest(sarima_1)
Box.test(sarima_1$residuals, lag=7)


coeftest(sarima_2)
Box.test(sarima_2$residuals, lag=7)

accuracy(sarima_1)
accuracy(sarima_2)

####################################
# Forecasting with SARIMA(1.0.0)(1,1,1)[7]
####################################
arima_forecast = forecast(sarima_1, h=12)
plot(arima_forecast, xlab="Date", ylab="Gross")


###### Resolve 'Figure margines too large' issue
dev.off()
par("mar")
par(mar=c(3,3,3,3))


