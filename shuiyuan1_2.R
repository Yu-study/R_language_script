#this script is based R 3.4.2 
#this script is used to analyze source of tax
#TIME: 2017-11-6 17:24:XX
rm(list = ls())
library(RODBC)
#library(xlsx)
#library(openxlsx)
#library(gdata)
#library(readxl)
#s2_17step2<-odbcConnectExcel2007("new2017s2.xlsx")
options(digits = 4)

s2_17step2<-read.csv("new2017s2.csv")
s2_17type2<-read.csv("new2017s2.csv")
except.vector2<-NULL
No<-nrow(s2_17step2)
ID<-1:No
except.vector2<-NULL
s2_17step2<-data.frame(ID,s2_17step2)
#show names of rows
#names(s2_17step2)
#No.1
except2<-which(s2_17step2$TA<5)
except.vector2<-c(except.vector2,s2_17step2[except2,]$ID)
write.csv(s2_17type2[except2,],"shuiyuan2/No_2_1.csv")
#No.1
#No.2
except2<-which(s2_17step2$OR>0&s2_17step2$TTP==0)
except.vector2<-c(except.vector2,s2_17step2[except2,]$ID)
write.csv(s2_17type2[except2,],"shuiyuan2/No_2_2.csv")
#No.3
except2<-which(s2_17step2$wage==0&s2_17step2$employment>0)
except.vector2<-c(except.vector2,s2_17step2[except2,]$ID)
write.csv(s2_17type2[except2,],"shuiyuan2/No_2_3.csv")


#write.csv(s2_17step2,paste("ttt",1,".csv"))
except.vector2s<-sort(except.vector2)
#final result
s2.result1_1<-s2_17type2[-except.vector2s,]
s2.result2_1<-s2_17type2[except.vector2s,]
write.csv(s2.result1_1,"result1_1.csv")
write.csv(s2.result2_1,"result2_1.csv")