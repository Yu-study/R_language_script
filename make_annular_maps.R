# This script is used to output a chart of cycle of ggplot2
library(rvest)
library(dplyr)
library(stringr)
library(showtext)
library(Cairo)
library(RColorBrewer)
library(ggplot2)
library(grid)


# download font
url<-"https://github.com/haoyuns/EyesAsia"
table<-read_html(url,encoding="utf-8")%>%html_table()
View(table)

table <- table[[2]]
View(table)

##delete NULL in “lowercase”
table1<-table[table$lowercase!="",]
##选择“lowercase”中空值数据所在行的所有数据，然后再用[,3:4]选择第三和第四列的数据。
##choose NULL of "lowercase"
table2<-table[table$lowercase=="",]%>%.[,3:4]
##重命名table1、table2中的列名，且重新赋值，便于整合成一维数据。
##rename tableX's colname
table11<-table1[,1:2]%>%rename(case=lowercase)
table12<-table1[,3:4]%>%rename(case=UPPERCASE)
table13<-table2%>%rename(case=UPPERCASE)
## combine data of tableXX
tabledata<-rbind(table11,table12,table13)

##筛选中文名称
##filtrate Chinese name
a <- str_split_fixed(tabledata$Content," ",n=3)[,3]
b <- str_split_fixed(tabledata$Content," ",n=3)[,2]
a[which(a %in% "")] <- b[which(a %in% "")] 
tabledata$Cname <- a
##筛选英文名称
##filtrate English name
a <- str_split_fixed(tabledata$Content," ",n=3)[,3]
c <- str_split_fixed(tabledata$Content," ",n=3)[,1]
c[which(a %in% "")] <- paste(c[which(a %in% "")],b[-(which(a %in% ""))],sep="")
tabledata$Ename <- c
##剔除不需要的列和地名，只需要中国省级行政区
tabledata<-tabledata[,-2]
word<-c("日本","蒙古","朝鲜","韩国","青海湖","鄱阳湖","洞庭湖","太湖","洪泽湖")
tabledata$m<-tabledata$Cname %in% word
tabledata<-tabledata%>%filter(m==FALSE)%>%.[,1:3]
##保存数据
#write.csv(tabledata,file = "tabledata")
write.csv(tabledata,file = "tabledata.csv",row.names=FALSE)

#make up a chart
#step1:surrounded font annulus
##生成一个虚拟指标，并分割为有序分段因子变量。
###使用runif(34,20,50)函数生产随机数据，其中34是数据量，20是最小值，50是最大值。
mymapdata<-read.csv("tabledata.csv",stringsAsFactors=FALSE,check.names=FALSE)
mymapdata<-transform(tabledata,scale=5,peform=runif(34,20,50))
mymapdata$scale<-as.numeric(mymapdata$scale)
###对peform数据进行分类
mymapdata$group<-cut(mymapdata$peform,breaks=c(20,26,32,38,44,50),levels=,labels=c("20~26","26~32","32~38","38~44","44~50"),order=TRUE)
###对mymapdata按照peform进行降序排列，同时添加order顺序。
mymapdata<-arrange(mymapdata,desc(peform));mymapdata$order=1:nrow(mymapdata)
mymapdata$order<-as.numeric(mymapdata$order)

#load font and chart
###将字体文件“EyesAsia-Regular”进行解压
unzip("EyesAsia-Regular.zip");
###将地图字体载入程序中，字体名字为“EyesAsia-Regular”
library(showtext);
font_add("EyesAsia-Regular", "EyesAsia-Regular.otf");
###作图函数
####打开图形设备
CairoPNG("chineserador.png",900,900)
####开始使用showtext
showtext_begin()
####开始制图
ggplot(mymapdata,aes(order,scale,label=case))+
  ylim(-6,6)+
  coord_polar(theta="x",start=0)+
  geom_text(aes(colour=group),family="EyesAsia-Regular",size=20)+
  scale_colour_brewer(palette="Blues",guide=FALSE)+
  theme_minimal()+
  theme(
    panel.grid=element_blank(),
    axis.title=element_blank(),
    axis.text=element_blank(),
  )
####停止使用showtext
showtext_end()
####关闭图形设备
dev.off()

#step2 make central discrete 
library(plyr)
library(maptools)
library(scales)
library(jsonlite)
library(jsonview)
library(readxl)
chinamapdata <- read_excel("F:/chinamapdata.xlsx")
chinamapdata

p2<-ggplot(chinamapdata,aes(long,lat,group=group))+geom_polygon(col="white",fill="grey")+
  coord_map("polyconic")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )


#step3 combine maos
##标签添加角度偏移量
circle<-seq(0,95,length=9)
circleALL<-rep(c(-circle,rev(circle[2:9])),2)
mymapdata1$circle<-circleALL
##ggplot极坐标下的首尾不衔接的缺陷，这里再插补一个缺失值
mymapdata<-arrange(mymapdata,order)
mapx<-mymapdata[mymapdata$order==34,]
mapx$order<-35;mapx$Cname=NA;mapx$case=NA
mymapdata1$order<-36;mapx$Cname=NA;mapx$case=NA
mymapdata1<-rbind(mymapdata1,mapx)
##加载微软雅黑和字体地图
windowsFonts(
  myfont=windowsFont("EyesAsia-Regular"),
  yahei=windowsFont("微软雅黑")
)
###作图函数
p1 <-ggplot(mymapdata1,aes(x=order,y=scale))+
  ylim(-6,7.5)+
  coord_polar(theta="x",start=0)+
  geom_text(aes(colour=group,label=case),family="myfont",size=10)+
  geom_text(aes(y=scale+2,angle=circle,label=Cname),family="yahei",size=3,vjust=0.5,hjust=.5)+
  scale_colour_brewer(palette="Blues",guide=FALSE)+
  theme_minimal()+
  theme(
    panel.grid=element_blank(),
    axis.title=element_blank(),
    axis.text=element_blank(),
  )

unzip("EyesAsia-Regular.zip")
font.add("myfont", "EyesAsia-Regular.otf")
font.add("yahei", "msyh.ttf")
CairoPNG("chineserador3.png",1000,1000)
showtext.begin()
vs <- viewport(width=0.8,height=0.95,x=0.5,y=0.5)    
print(p1,vp=vs)  
vs <- viewport(width=0.45,height=0.8,x=0.5,y=0.5)   
print(p2,vp=vs) 
showtext.end()
dev.off()