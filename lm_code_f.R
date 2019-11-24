
################################ Loading required libraries ##########################################

 # Data visualization
library(readxl) # CSV file I/O, e.g. the read_csv function
library(Hmisc)
library(corrplot)
library(forecast)

########################## setting directory #####################################
setwd('C:/Users/SESA529970/Documents/DS Academy/week 8')

######################################### Reading data ############################################
## Intuitive introduction

data<-read_excel("scores_data.xlsx")
head(data)

x<-data[,c(1,2)] ##independent variables

y<-data$Writing  ## dependent 

#Equation: y= b1+b2x1+b3x2
#To be estimated b1,b2,b3

x<-as.matrix(cbind(1,x)) ##(1363,3)

head(x)

beta<-as.matrix(c(0,0,0)) ## (b1,b2,b3)

m<-nrow(x)


cost<- sum(((x%*%beta)- y)^2)/(2*m) # Performance measure


alpha<-0.0001  ## small alpha gradient descent can be slow
## if alpha is too large then there will be no convergence

iterations<-40000


cost_fn<- function(beta)
{
  cost<- sum(((x%*%beta)- y)^2)/(2*m)
}


cost_it<-vector()
Sys.time()

for(i in 1:iterations)
{
  h=x%*%beta ##predicted
  loss=h-y
  gradient=t(x) %*% (loss) / m
  beta=beta-alpha*gradient  ## new_weight = existing_weight - learning_rate * gradient
  cost<-cost_fn(beta)
  cost_it[i]<-cost
}

Sys.time()

plot(cost_it)

print(beta)

### writing= beta[1,]+beta[2,]*x[,2]+beta[3]*x[,3]


cost_it[40000]

### based on the last iteration

final_beta<-beta

residuals<-(y-x%*%final_beta)  ## writing=0.090*Math+0.899*Reading+-0.21

fitted<-x%*%final_beta



plot(fitted,residuals,xlab="fitted",ylab="residuals")
abline(0,0)



################################################################################################

############################### Reading file ###################################################

house<-read.csv("house_data.csv")

head(house)
model_var<-c('price', 'sqft', 'bedrooms','baths')

correl_1<- house[,model_var]

res<-rcorr(as.matrix(correl_1))


corrplot(res$r)

plot(house$price~house$sqft, ylab="SalePrice", xlab= "Size", main="Size vs SalePrice", col="red", pch=18)



train.index <- sample(c(1:dim(house)[1]), dim(house)[1]*0.8)

model_train <- house[train.index,]
model_valid <- house[-train.index,]


model<-stats::lm(price~sqft,data=model_train)

summary(model)

## R^2= (1-sum((y-model$fitted)^2)/sum((y-mean(y))^2)


plot(model$fitted,model$resid,  xlab = "fitted w sqft", ylab = "resid", pch = 18)
abline(h = 0)

###1.Residuals have higher variability for positive values
###2.Additionally, variability of the residuals increases for larger fitted observations

par(mfrow=c(1,2))
plot(log(house$price)~house$sqft, ylab="log_SalePrice", xlab= "Size", main="Size vs log_SalePrice", col="red", pch=18)

plot(house$price~house$sqft, ylab="SalePrice", xlab= "Size", main="Size vs SalePrice", col="red", pch=18)


model_1<-stats::lm(log(price) ~ sqft,data=model_train)
summary(model_1)
plot(model_1$fitted,model_1$resid,  xlab = "fitted w sqft", ylab = "resid", pch = 18)
abline(h = 0)

pred<-predict(model_1,model_valid)

forecast::accuracy(pred,log(model_valid$price))
