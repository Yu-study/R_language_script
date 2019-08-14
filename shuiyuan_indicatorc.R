#this script is based R 3.4.2 
#this script is used to analyze source of tax
#TIME: 2017-11-6 17:24:XX
rm(list = ls())
library(RODBC)
#library(xlsx)
#library(openxlsx)
#library(gdata)
#library(readxl)
#s2_17step1<-odbcConnectExcel2007("new2017s2.xlsx")
indicator<-read.csv("resultf.csv")
NNNN<-row(indicator)
indicatorc<-rep(NULL,NNNN)
#s2_17type1<-read.csv("new2017s2.csv")
#names(indicator)
#calcualte the indeicators as we need
#2费用合计TE=销售费用SE+管理费用GAE+财务费用FE
TE<-indicator$SE+indicator$GAE+indicator$FE
indicatorc<-data.frame(TE)
#3盈亏户数比（税前）=利润总额大于0的企业数÷利润总额小于等于0的企业数
n1<-length(which(indicator$Total_profit>0))
n2<-length(which(indicator$Total_profit<0))
#4盈亏总额比（税前）==将大于0的利润总额加和÷将小于等于0的利润总额加和
ID.1<-which(indicator$Total_profit>0)
SSS1<-sum(indicator[ID.1,]$Total_profit)
ID.2<-which(indicator$Total_profit<0)
SSS2<-sum(indicator[ID.1,]$Total_profit)
PL_Nr_BT<-SSS1/SSS2
#5盈亏户数比（税后）=利润总额大于0的企业数÷利润总额小于等于0的企业数
n1<-length(which(indicator$NP>0))
n2<-length(which(indicator$NP<0))
PL_Nr_AT<-n1/n2
#6盈亏总额比（税后）==将大于0的利润总额加和÷将小于等于0的利润总额加和
ID.1<-which(indicator$NP>0)
SSS1<-sum(indicator[ID.1,]$NP)
ID.2<-which(indicator$NP<0)
SSS2<-sum(indicator[ID.1,]$NP)
PL_Nr_AT<-SSS1/SSS2
#7净资产收益率ROE=净利润NP÷所有者权益合计equity
ROE<-indicator$NP/indicator$equity
indicatorc<-data.frame(indicatorc,ROE)
#8总资产收益率ROA=净利润NP÷资产总计TA
ROA<-indicator$NP/indicator$TA
indicatorc<-data.frame(indicatorc,ROE)
#9成本费用率C_ratio=费用合计TE÷营业收入合计OR
C_ratio<-TE/indicator$OR
indicatorc<-data.frame(indicatorc,C_ratio)
#10经营利润率OPM=利润总额Total_profit÷营业收入合计OR
OPM<-indicator$Total_profit/indicator$OR
indicatorc<-data.frame(indicatorc,OPM)
#11总资产周转率TTA=营业收入OR÷资产合计TA
TTA<-indicator$OR/indicator$TA
indicatorc<-data.frame(indicatorc,TTA)
#12流动资产周转率TCA=营业收入合计OR÷流动资产CA
TCA<-indicator$OR/indicator$CA
indicatorc<-data.frame(indicatorc,TCA)
#13销售毛利率GPM=（营业收入OR-营业成本OC）÷营业收入OR
GPM<-(indicator$OR-indicator$OC)/indicator$OR
indicatorc<-data.frame(indicatorc,GPM)
#14资产负债率AL_ratio=负债合计TL÷资产合计TA
AL_ratio<-indicator$TL/indicator$TA
indicatorc<-data.frame(indicatorc,AL_ratio)
#15所有者权益比率EA_ratio=所有者权益合计equity÷资产合计TA
EA_ratio<-indicator$equity/indicator$TA
indicatorc<-data.frame(indicatorc,EA_ratio)
#16增值税占比VAT_ratio=实缴增值税Paid_in_VAT÷实缴税金总额TTP
VAT_ratio<-indicator$Paid_in_VAT/indicator$TTP
indicatorc<-data.frame(indicatorc,VAT_ratio)
#17企业所得税占比PEIT=实缴企业所得税PCIT÷实缴税金总额TTP
PEIT<-indicator$PCIT/indicator$TTP
indicatorc<-data.frame(indicatorc,PEIT)
#18进口关税占比import_tariff_ratio=进口关税import_tariff÷实缴税金总额TTP
import_tariff_ratio<-indicator$import_tariff/indicator$TTP
indicatorc<-data.frame(indicatorc,import_tariff_ratio)
#19进口环节增值税占比import_VAT_ratio=进口环节增值税import_VAT÷实缴税金总额TTP
import_VAT_ratio<-indicator$import_VAT/indicator$TTP
indicatorc<-data.frame(indicatorc,import_VAT_ratio)
#20总体税负OTB=实缴税金总额TTP÷营业收入合计OR
OTB<-indicator$TTP/indicator$OR
indicatorc<-data.frame(indicatorc,OTB)
#21增值税税负VATB=实缴增值税Paid_in_VAT÷营业收入合计OR
VATB<-indicator$Paid_in_VAT/indicator$OR
indicatorc<-data.frame(indicatorc,VATB)
#22企业所得税PCITB=实缴企业所得税PCIT÷营业收入合计OR
PCITB<-indicator$PCIT/indicator$OR
indicatorc<-data.frame(indicatorc,PCITB)
#23企业增加值EVA=固定资产折旧DFA+工资薪金支出wage+应交增值税VAT+营业利润OP+本季度缴纳的排污费CDP
EVA<-indicator$DFA+indicator$wage+indicator$VAT+indicator$OP+indicator$CDP
indicatorc<-data.frame(indicatorc,EVA)

#final csv
result_FF<-data.frame(indicator,indicatorc)
write.csv(result_FF,"shuiyuan/result_final.csv")





