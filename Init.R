# �Ӱ�����ѯ�� ��R��������ڴ����ݴ����� - ������ƪ�� ��v1.0)
# ����޸�ʱ�䣺2014-04-23(17.3)

# �����Լ����ذ�װ��Ӧ����չ�������ǵ�֧��������


#setwd("D:/RWD/R-FBD")

library(quantmod)    
library(ggplot2) 
library(reshape2)
library(plyr)
library(tseries)
library(timeSeries)
library(PerformanceAnalytics)
library(nlme)


# ���ڽ���ggplot2������ʱ����ʹ�õ�����
data(diamonds)
data(economics)
data(msleep)

dsmall <- diamonds[sample(nrow(diamonds),100),]
dlarge <- diamonds[sample(nrow(diamonds),1000),]

pt <- function() { szCurTime <- as.character.Date(Sys.time()); options(prompt=paste(szCurTime,">",sep="")) }
pt()

print("��ʼ����ɣ�",quote=F)