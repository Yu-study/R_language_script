#load packages
require(plm)
#read data
pipcdata<-read.csv("C:/Users/Administrator/Desktop/pipc.csv",header = TRUE)
names(pipcdata)
#tranform into penal data
pricepenal<-plm.data(pricedata,indexes = c("city_id","year"))
#equation of regression
equation<-housing_price ~ GDP + population + averwage + investment + acreage
pem<-plm(equation,data=pricepenal,model="pooling")
pooltest(equation,data=pricepenal,effect="individual",method="within")
pooltest(equation,data=pricepenal,effect="time",method="within")
fem<-plm(equation,data=pricepenal,model="within",effect = "individual")
#choose model
pFtest(fem,pem)

rem<-plm(equation,data=pricepenal,model="random",random.method = "swar")
#choose model fixed effect or random effect
phtest(fem,rem)
pbgtest(equation,data=pricepenal,model = "within")

summary(fem)
#Wooldridge test
pwartest(equation,data=pricedata)
#estimate the interception
fixef(fem,effect="individual")
summary(fixef(fem,type="level"))
#ggplot2 part II
require(ggplot2)
require(scales)
pricet<-pricedata[316:333,]
pic<-ggplot(pricet,aes(x=year,y=housing_price))
#pic<-pic+geom_boxplot(aes(color=factor(D),group=D))
pic<-pic+geom_line(aes(color=factor(D),group=D))
pic<-pic+scale_color_discrete(name="city level")
pic<-pic+scale_y_continuous(labels = comma)
pic<-pic+labs(title="housing Price",y="price",x="year")