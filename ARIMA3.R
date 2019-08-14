#load packages we may use
library(forecast)
library(zoo)
library(FinTS)
library(fGarch)
library(rugarch)
#input data
IF <- read.csv("C:/Users/Administrator/Desktop/IF.csv")
#check some data
head(IF)
#transform the type of data(data.frame into time series data)
IFC<-zoo(IF[,2],IF[,13])
plot(IFC, type="l",main="IF", xlab="Time",ylab="Close Price")  

#IFdifference1 <- diff( (IF[,2]), differences=1)
IFdifference1<-diff(ts(IF[,2]),lag=1)
plot(IFdifference1)
#check the stationarity
library(tseries)
adf.test(IFdifference1)
acf(IFdifference1, lag.max=30)
acf(IFdifference1, lag.max=30,plot=FALSE)

pacf(IFdifference1, lag.max=30)
pacf(IFdifference1, lag.max=30,plot=FALSE)


arimafit <- auto.arima(IFdifference1)
arimafit 

res1<-residuals(arimafit)
dev.new()
tsdisplay(res1)

fit3 <- Arima(IFC, order=c(2,1,2))
res <- residuals(fit3)
dev.new()
tsdisplay(res)


#however, 

# by checking residuals, the serial correlation has not been removed yet,
# by checking ACF plot, it shows significant lag as at 7, so we choose order 7,1,2 to remove significant lag

#the most serial correlation is removed 
#finally, we decided 7,1,2 as final model.  
#Then ,we do arch effect test 


fit412 <- Arima(IFC, order=c(4,1,2))
summary(fit412)
res412 <- residuals(fit412)
dev.new()
tsdisplay(res412)


fit712 <- Arima(IFC, order=c(7,1,2))
summary(fit712)
res712 <- residuals(fit712)
dev.new()
tsdisplay(res712)

# we can see res712 is better since only acf lag 14 is signifcant. so we use ARIMA 712,


# now it is time to test arch effect
AutocorTest(res712)
ArchTest(res712)

# F test is very large, P is small, so strong arch effect

dev.new()
tsdisplay(res712^2)

# we can see strong acf serial correlation 

tsdisplay(IF[,2])
# by looking the PACF order of IF, the lag 2 is significant, so choose arch model (2,0)


library(fGarch)

arch16<-garch(res712,order=c(0,16),res712,trace=F)
summary(arch16)

garch11<-garchFit(~arma(7,1,2)+garch(1,1),data=IFC,trace=F)
summary(garch11)

garch11<-garchFit(~garch(1,1),data=res712,trace=F)
summary(garch11)

garch11=garchFit(~arma(7,2)+garch(1,1), data=IFdifference1,algorithm="nlminb+nm",trace=F)
summary(garch11)

# -4230


# -4233


 install.packages("rugarch")
 library(rugarch)


 plot( ugarchforecast(garch11,n.ahead=20))

 plot(garch11)
 lines(IFdifference1)



