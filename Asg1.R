#Generating samples of sizes 10,10^2,...,10^6

sample1 <- sample(2:12, size=10, replace = TRUE)
sample2 <- sample(2:12, size=10^2, replace = TRUE)
sample3 <- sample(2:12, size=10^3, replace = TRUE)
sample4 <- sample(2:12, size=10^4, replace = TRUE)
sample5 <- sample(2:12, size=10^5, replace = TRUE)
sample6 <- sample(2:12, size=10^6, replace = TRUE)

#Visually verifying whether the sample distribution looks as expected

library(ggplot2)
qplot(sample1,binwidth=1)
qplot(sample2,binwidth=1)
qplot(sample3,binwidth=1)
qplot(sample4,binwidth=1)
qplot(sample5,binwidth=1)
qplot(sample6,binwidth=1)

#Frequency Calculation and copying result set into new atomic vectors

freqSample1 <- 0;freqSample2 <- 0;freqSample3 <- 0;freqSample4 <- 0;freqSample5 <- 0;freqSample6 <- 0;
for(i in 1:11) {
  freqSample1[i] <- sum(sample1==(i+1));freqSample2[i] <- sum(sample2==(i+1));freqSample3[i] <- sum(sample3==(i+1));
  freqSample4[i] <- sum(sample4==(i+1));freqSample5[i] <- sum(sample5==(i+1));freqSample6[i] <- sum(sample6==(i+1));
}

expProbabilisticVector <- c(1,2,3,4,5,6,5,4,3,2,1)/36


#RMSE calculation for each of the samples

rmseSample1 <- sqrt(mean((freqSample1/10-expProbabilisticVector)^2))
rmseSample2 <- sqrt(mean((freqSample2/100-expProbabilisticVector)^2))
rmseSample3 <- sqrt(mean((freqSample3/1000-expProbabilisticVector)^2))
rmseSample4 <- sqrt(mean((freqSample4/10000-expProbabilisticVector)^2))
rmseSample5 <- sqrt(mean((freqSample5/100000-expProbabilisticVector)^2))
rmseSample6 <- sqrt(mean((freqSample6/1000000-expProbabilisticVector)^2))

#Plotting rmses
qplot(c(rmseSample1,rmseSample2,rmseSample3,rmseSample4,rmseSample5,rmseSample6),c(10,10^2,10^3,10^4,10^5,10^6))
