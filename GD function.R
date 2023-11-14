
################################ Loading required libraries ##########################################

 # Data visualization
library(readxl) # CSV file I/O, e.g. the read_csv function
library(Hmisc)
library(corrplot)
library(forecast)

########################## setting directory #####################################
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

