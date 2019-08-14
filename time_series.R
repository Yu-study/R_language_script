passenger = read.csv('passenger.csv',header=F,sep=' ')
p<-unlist(passenger)

#�����ݱ��time series��  frequency=12��ʾ���·�Ϊ��λ��time series. start ��ʾʱ�俪ʼ�㣬������c(a,b,...)��ʾ��  ���簴��Ϊ��λ����׼�������� start=c(2011,1) ��ʾ��2011��1�¿�ʼ
#���Ҫ��ʾ����ģ������� ts(p,frequency=7,start=c(1,1))  �ܶ���ϲ���� ts(p,frequency=365,start=(2011,1))���������и���������û�а����ڶ���
pt<-ts(p,frequency=12,start=2001) #
plot(pt)
train<-window(pt,start=2001,end=2011+11/12)
test<-window(pt,start=2012)

library(forecast)
pred_meanf<-meanf(train,h=12)
rmse(test,pred_meanf$mean) #226.2657

pred_naive<-naive(train,h=12)
rmse(pred_naive$mean,test)#102.9765

pred_snaive<-snaive(train,h=12)
rmse(pred_snaive$mean,test)#50.70832

pred_rwf<-rwf(train,h=12, drift=T)
rmse(pred_rwf$mean,test)#92.66636

pred_ses <- ses(train,h=12,initial='simple',alpha=0.2)
rmse(pred_ses$mean,test) #89.77035

pred_holt<-holt(train,h=12,damped=F,initial="simple",beta=0.65)
rmse(pred_holt$mean,test)#76.86677  without beta=0.65 it would be 84.41239

pred_hw<-hw(train,h=12,seasonal='multiplicative')
rmse(pred_hw$mean,test)#16.36156

fit<-ets(train)
accuracy(predict(fit,12),test) #24.390252


pred_stlf<-stlf(train)
rmse(pred_stlf$mean,test)#22.07215

plot(stl(train,s.window="periodic"))  #Seasonal Decomposition of Time Series by Loess

fit<-auto.arima(train)
accuracy(forecast(fit,h=12),test) #23.538735

ma = arima(train, order = c(0, 1, 3),   seasonal=list(order=c(0,1,3), period=12))
p<-predict(ma,12)
accuracy(p$pred,test)  #18.55567
BT = Box.test(ma$residuals, lag=30, type = "Ljung-Box", fitdf=2)


wape = function(pred,test)
{ 
  len<-length(pred) 
  errSum<-sum(abs(pred[1:len]-test[1:len])) 
  corSum<-sum(test[1:len])
  result<-errSum/corSum
  result
}


mae = function(pred,test)
{ 
  errSum<-mean(abs(pred-test))    #ע��  ��wape��ʵ������ǲ��Ǽ��˺ܶ�
  errSum
}

rmse = function(pred,test)
{ 
  res<- sqrt(mean((pred-test)^2) )
  res
}