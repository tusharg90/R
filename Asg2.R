#Assignment 2

#Function definition:
my_rmse <- function(v1,v2,na.rm=F){
  if(!is.numeric(v1))
    stop("Invalid vector v1")
  if(!is.numeric(v2))
    stop("Invalid vector v2")
  
  if(na.rm==F){
    if(any(is.na(v1)))
      stop("Invalid vector v1 with NA")
    if(any(is.na(v2)))
      stop("Invalid vector v2 with NA")
    sqrt(mean((v1-v2)^2))
  }
  else{
    sqrt(mean((v1[!is.na(v1 & v2)] - v2[!is.na(v1 & v2)])^2))
  }
}


library(Metrics)
#Test cases:
#1 Non-numeric, Numeric, F
rmse(c("ABC","ZY",1,2), c(1,2,3,4))#Expected
my_rmse(c("ABC","ZY",1,2), c(1,2,3,4))#Actual


#2 Numeric, Non-Numeric, F
rmse(c(1,2,3,4), c("ABC",1,2,4))#Expected
my_rmse(c(1,2,3,4), c("ABC",1,2,4))#Actual

#3 Numeric(with NAs), Numeric(no	NAs), F
rmse(c(1,2,3,NA), c(5,1,2,4))#Expected
my_rmse(c(1,2,3,NA), c(5,1,2,4))#Actual

#4 Numeric(no	NAs), Numeric(with NAs), F
rmse(c(1,2,3,4), c(NA,1,2,4))#Expected
my_rmse(c(1,2,3,4), c(NA,1,2,4))#Actual

#5 Numeric(no	NAs), Numeric(no	NAs), F
rmse(c(1,2,3,4), c(5,1,2,3))#Expected
my_rmse(c(1,2,3,4), c(5,1,2,3))#Actual

#6 Numeric(with	NAs), Numeric(with	NAs), T
rmse(c(NA,2,3,4), c(5,NA,2,4))#Expected
my_rmse(c(NA,2,3,4), c(5,NA,2,4),T)#Actual