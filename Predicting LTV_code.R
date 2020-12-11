##--------------Predicting the LTV of first year customers------------------------##

##Author:Vidya shree Suresh
##Date : 7/14/2018

##-------------------Required packages---------------------------##
library(readxl)
library(base)
library(Hmisc)
library(randomForest)
library(dplyr)
library(MASS)
library(MLmetrics)
install.packages("rfUtilities")
library(rfUtilities)
##-----------------------Reading and understanding dataset--------------------------##



train_dataset<-read_excel('C:/Users/vidya.suresh/Documents/Personal/Lifetime_value.xlsx')
test_dataset<-read_excel('C:/Users/vidya.suresh/Documents/Personal/LTV_ test_set.xlsx')

dataset<-rbind(train_dataset,test_dataset)

## checking for missing values in the dataset
sum(is.na(dataset))

## Quick view of dataset
head(dataset)
str(dataset)


## Created a booking month column in excel converting that to a factor
dataset$`Booking_month`<-as.factor(dataset$`Booking_month`)

##Converting international flag to a factor

dataset$international<-as.factor(dataset$international)

dataset_cf_all=dataset %>% mutate_if(is.character, as.factor)
str(dataset_cf_all)

dataset_cf<-dataset_cf_all[1:441,]



## understanding descriptive statistics in r
summary(dataset_cf)



## Notes: booking months between Jan and July

hist(dataset$commission) ## Both are right skewed
hist(dataset$length_of_stay)

## To get the same factor level for training and test split in the file

##Getting correlation values
rcor<-rcorr(as.matrix(dataset_f[,c(3,5,6)]),type="pearson")

#                   length_of_stay commission  ltv
# length_of_stay           1.00       0.91 0.91
# commission               0.91       1.00 1.00
# ltv                      0.91       1.00 1.00



###---------------Selection of variables----------------------------------------##


rf<-randomForest(ltv~.,data=dataset_cf[,-c(1,6)],importance=T,ntree=1000
)

## Checking for variable importance,lenght of stay followed by booking country are the
##two most important variables for prediction

varImpPlot(rf)

importance(rf,type=1)


##-----------------------Prediction and model diagnostics-----------------------------##

## Splitting the dataset into training and testing set
final_data<-dataset_cf[,-c(1,6)]

write.csv(final_data,"for_brick.csv")
rows<-sample(nrow(final_data)*0.7)

train<-final_data[rows,]
test<-final_data[-rows,]

## Building the model with training data

rf.model<-randomForest(ltv~.,data=train,importance=T,ntree=1500)


##summary(rf.model)

rf.pred<-predict(rf.model_trial, test)

mape(test$ltv,rf.pred)

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}


R2 <- 1 - (sum((test$ltv-rf.pred)^2)/sum((test$ltv-mean(rf.pred))^2))
R2

## K cross validation


rf.cv<-rf.crossValidation(rf.model,xdata=train,ntree=1500)
par(mfrow=c(2,2))
plot(rf.cv)  
plot(rf.cv, stat = "mse")
plot(rf.cv, stat = "var.exp")
plot(rf.cv, stat = "mae")

##-----Reading test set for prediction---------------------##


test.pred<-predict(rf.model, dataset_cf_all[442:460,-c(1,6,8)])
test.pred

write.csv(test.pred,"results_ltv_2.csv",row.names = F)
