library(ggplot2)
library(plyr)
library(ggthemes)
library(rgdal)
library(dplyr)
library(Cairo) #export picture HD
library(mapproj)
library(maptools)
china_map<-readOGR("G:/Yu_study/GIS/province/bou2_4p.shp",stringsAsFactors=FALSE)

#transform coordinate from GSW 84 to policonic
mymap=ggplot(data = fortify(china_map))+geom_polygon(aes(x=long,y=lat,group=id),
                                             colour="black",fill=NA)+theme_grey()+coord_map("polyconic")

print(mymap)

#read provinces' data
#province<- china_map@data 
province<-china_map@data["NAME"]

#we find 925 polygon
#provinces <- data.frame(province,id=seq(0:924)-1)   
province$id<-0:924;
province[province$id==898,"NAME"]<-"?????ر???????"
#transform it into data.frame 1st way
mymapdata<-fortify(china_map)
mymapdata$id<-as.numeric(mymapdata$id)
mymapdata<-merge(mymapdata,province,all.x=TRUE)
#mymapdata<-join(mymapdata,mydata,type="full")       

#transform it into data.frame 2nd way
#china_map_data <- fortify(china_map)          

#china_map_data <- join(china_map_data, provinces, type = "full")       #?ϲ?��?????ݿ?

#find names of provinces
unique(mymapdata$NAME) 

#read data which you want to visualize
mydata <- read.csv("map_use/papermap2.csv")    
#merge two dataset
china_data <- join(mymapdata, mydata, type="full")       

ggplot(china_data, aes(x = long, y = lat, group = group, fill = ABS)) +
  geom_polygon(colour="grey40") +
  scale_fill_gradient(low="white",high="steelblue") +  #ָ??????????ɫ????ʹ??RGB
  coord_map("polyconic")        #ָ??ͶӰ??ʽΪpolyconic?????ó????ӽ??й???ͼ

ggplot(china_data, aes(x = long, y = lat, group = group,fill = ABS)) +
  geom_polygon(colour="grey40") +
  scale_fill_gradient(low="white",high="grey") +  #ָ??????????ɫ????ʹ??RGB
  coord_map("polyconic") +       #ָ??ͶӰ??ʽΪpolyconic?????ó????ӽ??й???ͼ
  theme(               #????????Ҫ??Ԫ??
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.2,0.3)
  )

#################################################################################
midpos <- function(x) mean(range(x,na.rm=TRUE)) #ȡ??״?ڵ?ƽ??????
centres <- ddply(china_data,.(province),colwise(midpos,.(long,lat)))

ggplot(china_data,aes(long,lat))+       #?˴??﷨??ǰ?治ͬ???ο?ggplot2һ??P85
  geom_polygon(aes(group=group,fill=ABS),colour="black")+
  scale_fill_gradient(low="white",high="steelblue") +
  coord_map("polyconic") +
  geom_text(aes(label=province),data=centres) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
#######################################################################################

province_city <- read.csv("G:/Yu_study/GIS/chinaprovincecity.csv")  #??ȡʡ??????????

windowsFonts(myFont = windowsFont("΢???ź?"))
ggplot(china_data,aes(long,lat))+
  geom_polygon(aes(group=group,fill=ABS),colour="grey60")+
  scale_fill_gradient(low="white",high="steelblue") +
  coord_map("polyconic") +
  geom_text(aes(x = jd,y = wd,label = province), data=province_city)+
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.3,0.30),
    legend.background = element_blank(),
    legend.text.align=1,
    legend.key.width=unit(1,"line"),
    legend.key.height=unit(1,"line")
  )


#another version
qa <- quantile(china_data$ABS[which(china_data$ABS!=0)],c(0,0.2,0.4,0.6,0.8,1.0))
china_data$zhibiao_q<-cut(china_data$ABS,qa,
                          labels= c("0-20%","20-40%","40-60%","60-80%","80-100%"),include.lowest = TRUE)
province_city$zhibao<-round(runif(34,50,100))
ggplot(china_data,aes(long,lat))+
  geom_polygon(aes(group=group,fill=zhibiao_q),colour="black",size=0.25)+
  scale_fill_brewer(palette="RdYlGn")+
  coord_map("polyconic") +
  guides(fill=guide_legend(reverse=TRUE,title=NULL))+
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.3,0.25),
    legend.background = element_blank(),
    legend.text.align=1,
    legend.key.width=unit(1,"line"),
    legend.key.height=unit(1,"line")
   )
####################################################################################################
#combine our data
#ch = fortify(x,region = 'NAME')
#
#ch = transform(ch,id=iconv(id,from = 'GBK'),group = iconv(group, from = 'GBK'))
#names(ch)[1:2] = c("x","y")
#######################################################################################################
##
#######################################using REmap#####################################################
library(REmap)
cc<-read.csv("map_use/papermap2.csv")
remapC(cc[,2:3],maptype = "china",title = "Geographic distribution of ABS issue",
       subtitle = "2012-2017 total issuance",theme=get_theme("Sky"),
       color = c("purple","lightblue"),maxdata = 3000000,mindata=0)

