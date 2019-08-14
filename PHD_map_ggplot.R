library(ggplot2)
library(plyr)
library(ggthemes)
library(rgdal)
library(dplyr)
library(mapproj)
# I have a *.shp file to combine my ggplot2 map geom
china_map<-readOGR("G:/Yu_study/GIS/province/bou2_4p.shp",stringsAsFactors=FALSE)
mydata<-china_map@data["NAME"]
mydata$id<-0:924;
mydata[mydata$id==898,"NAME"]<-"�����ر�������"

#library(broom)
#mymapdata<-tidy(china_map)
mymapdata<-fortify(china_map)
mymapdata$id<-as.numeric(mymapdata$id)
mymapdata<-merge(mymapdata,mydata,all.x=TRUE)
#mymapdata<-join(mymapdata,mydata,type="full")

#print a China map simple version
#mymap=ggplot(data = fortify(mymapdata))+geom_polygon(aes(x=long,y=lat,group=id),colour="black",fill=NA)+theme_grey()
#print(mymap+coord_map())

#we use %>% to transfer left value to right
mymapdata<- mymapdata %>%rename(region=NAME)
#mymapdata = transform(mymapdata,id=iconv(id,from = 'GBK'),group = iconv(group, from = 'GBK'))
#names(mymapdata)[1:2] = c("x","y")

#���ȹ���12����ݱ�����
mydata_new<-data.frame(NAME=unique(mydata$NAME))
for (i in 2:13){
  mydata_new[,i]<-round(runif(34,0,250))
}

#���岢�и�����������Ϊ���ӱ���
names(mydata_new)[2:length(mydata_new)]<-as.character(2001:2012)
mydata_new<-mydata_new%>%tidyr::gather(year,zhibiao,-1)
mydata_new$fact<-cut(mydata_new$zhibiao,breaks=c(0,50,100,150,200,250),
                     labels=c('0~50','50~100','100~150','150~200','200~250'),order=TRUE,include.lowest = TRUE)

#�����ͼһ��������ɫ��ͼ���� 
ggplot(mydata_new,aes(map_id=NAME,fill=fact))+
  geom_map(map=mymapdata,colour="grey65")+
  scale_fill_brewer(palette="Blues") +  ###Blues&Greens
  facet_wrap(~year)+
  expand_limits(x=mymapdata$long,y=mymapdata$lat)+
  coord_map("polyconic")+
  guides(fill=guide_legend(reverse=TRUE,title=NULL))+       
  theme_void()%+replace% theme(legend.position = c(0,0.7),legend.text.align=1)  

#���벢�ϲ�ʡ���������ľ�γ������
province_city <- read.csv("D:/R/rstudy/Prov++ince/chinaprovincecity.csv") 
province_city<-province_city%>%select(province,jd,wd)
mydata_new<-merge(mydata_new,province_city,by.x="NAME",by.y="province",all.x=TRUE)

#�����ͼ������ɫ���ݵ�ͼ����
ggplot(mydata_new,aes(map_id=NAME))+
  geom_map(map=mymapdata,colour="grey65",fill="#EEF3FA")+
  geom_point(aes(x=jd,y=wd,size=zhibiao,colour=zhibiao),shape=16)+
  scale_size_area(max_size=6) +  
  scale_colour_gradient(low="white",high="#D73434")+ 
  facet_wrap(~year)+
  expand_limits(x=mymapdata$long,y=mymapdata$lat)+
  coord_map("polyconic")+
  guides(fill=guide_legend(reverse=TRUE,title=NULL),size=guide_legend(reverse=TRUE,title=NULL))+       
  theme_void()%+replace% theme(legend.position = c(0,0.7),legend.text.align=1)   

#�����ͼ������ɫ����+��������ͼ���� 
ggplot(mydata_new,aes(map_id=NAME))+
  geom_map(map=mymapdata,aes(fill=fact),colour="white")+
  geom_point(aes(x=jd,y=wd,size=zhibiao,colour=zhibiao),shape=16)+
  scale_size_area(max_size=4) +  
  scale_fill_brewer(palette="Greens") +  ###Blues&Greens
  scale_colour_gradient(low="white",high="#D73434")+ 
  facet_wrap(~year)+
  expand_limits(x=mymapdata$long,y=mymapdata$lat)+
  coord_map("polyconic")+
  guides(fill=guide_legend(reverse=TRUE,title=NULL),size=guide_legend(reverse=TRUE,title=NULL))+       
  theme_void()%+replace% theme(legend.position = c(0,0.7),legend.text.align=1)  