#this script is just used to slove question 1.10
#this script is based on R version 3.4.2 (2017-09-28) -- "Short Summer"
#
#install.packages("fBasics")
#install.packages("TSA")
library(fBasics)
library(TSA)
#generate an independent Gaussian white noise {r_t}T,t=1 with T=100
#here we assume var=4 mean=0
T<-100
rt<-rnorm(100,0,2)
gt<-sample(rt,size=400,replace = TRUE)
gt<-ts(gt)
gt1<-gt[-1]
gt_1<-gt[-400]
ac1<-cor(gt_1,gt1)
#calculate statistic we need 
Z<-sqrt(T)*ac1
#Q3 &Q3s
Q3test<-Box.test(gt,lag=3,type ="Box-Pierce")
Q3s<-Q3test$statistic
Q3test_s<-Box.test(gt,lag=3,type ="Ljung-Box")
Q3<-Q3test_s$statistic
#Q6 &Q6s
Q6test<-Box.test(gt,lag=6,type ="Box-Pierce")
Q6s<-Q6test$statistic
Q6test_s<-Box.test(gt,lag=6,type ="Ljung-Box")
Q6<-Q6test_s$statistic
#Q12 &Q12s
Q12test<-Box.test(gt,lag=12,type ="Box-Pierce")
Q12s<-Q12test$statistic
Q12test_s<-Box.test(gt,lag=12,type ="Ljung-Box")
Q12<-Q12test_s$statistic

#create a histogram
hist(x=c(Z,Q3,Q3s,Q6))

XXX<-c(Z,Q3,Q3s,Q6,Q6s,Q12,Q12s)
#percentile
quantile(XXX,probs=c(0,0.5,1))

#calculate the theorical chi_square statistic
qchisq(0.95,3)
qchisq(0.95,6)
qchisq(0.95,12)
