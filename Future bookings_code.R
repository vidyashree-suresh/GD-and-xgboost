##--------------Predicting future bookings--------------------------------##

##Author:Vidya shree Suresh
##Date : 7/15/2018

##-------------------Required packages------------------------------------##
library(readxl)
library(base)
library(aTSA)
library(dplyr)
library(forecast)
library(stats)
library(earth)



input<-read_read_excel('C:/Users/vidya.suresh/Documents/Personal/Booking.com/Bookings_channel_spend.xlsx')
to_predict<-read_excel('C:/Users/vidya.suresh/Documents/Personal/Booking.com/To_be_predicted.xlsx')

##---------------------Preliminary data analysis------------------------------##

sum(is.na(input)) ## no missing values

str(input)

par(mfrow=c(2,2))

plot(input$sales)
plot(input$ch1_spend,type="l")
plot(input$ch2_spend,type="l")

rcor<-rcorr(as.matrix(input[,-1]),type="pearson")

#           sales ch1_spend ch2_spend
# sales      1.00      0.90      0.97
# ch1_spend  0.90      1.00      0.93
# ch2_spend  0.97      0.93      1.00

##--------------------------Decomposing the signal------------------------------##
bookings<-input$sales

ts_sales<-ts(bookings,frequency=12)
decompose_sales<-decompose(ts_sales,"additive")

plot(decompose_sales)

##Signal is seasonal through the time

##-----------------------------Stationarity and Auto-correlation tests-----------##

adf.test(ts_sales) ## non-stationary signal in fact, p.value = 0.01 means p.value <= 0.01 

#Even though auto.ARIMA can be used to reach optimal p,d,q values 
# ACF and PACF help with understanding the final equation

##ACF plots display correlation between a series and its lags

Acf(ts_sales)
Pacf(ts_sales) ## AR can be set to 1

## starting with d=1 
sales_df<-diff(ts_sales,differences=1)
plot(bookings_df)

adf.test(bookings_df)

Acf(bookings_df) #significant spike at 6 and 12 (seasonality)
Pacf(bookings_df) #significant spike at 6



##------------------ Predicting using ARIMAx model------------------------##

# splitting data into sales #

train_f<-input[1:30,]#c(1,2,0)
test_f<-input[31:36,]


#---getting initial sense of pd,q using auto.ARIMA------------------------##

Sales_pred<-auto.arima(ts_sales,approximation = FALSE)

summary(Sales_pred)

# Creating a matrix of regulatory variables #

reg<-cbind(train_f$ch1_spend,train_f$ch2_spend)

# -------------- Fitting ARIMAX model to predict future bookings ---------##

fit<-arima(train_f$sales,order=c(0,1,0),xreg=reg)

summary(fit)

reg_test<-cbind(test_f$ch1_spend,test_f$ch2_spend)

# Predicting to check the accuracy of the model ##
predicted_sales<-predict(fit,n.ahead=6,newxreg =reg_test)
predicted_sales

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

mape(test_f$sales,predicted_sales$pred) ## MAPE % of 1.25%

##------------Predicting for next 12 months--------------------------------##

## Using the order of p,d,q as 0,1,0

fit_complet<-arima(input$sales,order=c(0,1,0),xreg=cbind(input$ch1_spend,input$ch2_spend))
summary(fit_complet)


new_predict<-predict(fit_complet,n.ahead=12,newxreg=cbind(to_predict$ch1_spend,to_predict$ch2_spend))
new_predict$pred


write.csv(new_predict,"Future_bookings_predicted.csv")
