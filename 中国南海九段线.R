library(maptools)
library(ggplot2)
library(plyr)
library(grid)
library(RColorBrewer)
library(rgdal)
#setwd("C:/Users/Jie Zhang/Desktop/China_AreaScatter/")

china_map <- readOGR("G:/Yu_study/GIS/province/bou2_4p.shp")            # ??ȡ??ͼ?ռ?????
#china_map1 <- fortify(china_map)                               #ת??Ϊ???ݿ?
#china_map1$value<-runif(nrow(china_map1))
x <- china_map@data         
xs <- data.frame(id=row.names(x),x)

china_map0<- fortify(china_map)
china_map_data <- join(china_map0, xs, type = "full")
mydata <- read.csv("geshengzhibiao.csv")
china_data<-join(china_map_data,mydata,type="full") 
colnames(china_data)[ncol(china_data)]<-"Value"
china_map1<-china_data
NanHai <- read.csv("?й??Ϻ??Ŷ???.csv")             #??ȡҵ??????
colnames(NanHai)<-c("long","lat","ID")

china_map2<-china_map1[china_map1$long>106.55 & china_map1$long<123.58,]
china_map2<-china_map2[china_map2$lat>4.61 & china_map2$lat<25.45,]
#--------Method1----------------------------------------------------------------------------------
P_HaiNan<-ggplot()+
  geom_polygon(data=china_map2, aes(x=long, y=lat, group=group,fill=Value), colour="black",size=0.25)+ #?й???ͼ
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11,'Spectral')))(32))+
  geom_line(data=NanHai, aes(x=long, y=lat, group=ID), colour="black", size=1)+ #?й??Ϻ??Ŷ???
  geom_rect(aes(xmin=106.55, xmax=123.58, ymin=4.61, ymax=25.45),fill=NA,colour="black")+
  #coord_map("polyconic") +
  coord_cartesian(ylim=c(4.61,25.45),xlim=c(106.55,123.58))+
  theme_void()+
  theme(
     legend.position = "none"
  #   panel.background = element_blank(),
  #   plot.background= element_blank(),
  #   plot.margin = unit(c(0,0,0,0), "cm"),
  #   panel.spacing= unit(c(0,0,0,0), "cm")
   )


p_ChinaMain <- ggplot()+
  geom_polygon(data=china_map1, aes(x=long, y=lat, group=group,fill=Value),  colour="black",size=0.25)+ #?й???ͼ
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11,'Spectral')))(32))+#,
                       #breaks = seq(50,400,50),guide = guide_legend(title="value"))+
  #geom_point(data=china_data, aes(x = jd,y = wd,size=zhibiao), fill="black", alpha=1,shape=21, colour="white")+ #????ͼ
  #scale_size_area(max_size=8)+      
  #coord_map("polyconic") +
  #guides(fill=guide_legend(title="value"))+
  ylim(15,55)+
  theme(
  #   #panel.grid = element_blank(),
  #   panel.background = element_blank(),
  #   axis.text = element_blank(),
  #   axis.ticks = element_blank(),
  #   axis.title = element_blank(),
  #   legend.direction="vertical",
     legend.position=c(0.2,0.2),
     legend.background = element_blank()
  #   legend.text.align=0
   )
subvp <- viewport(x = 0.85, y = 0.23, width = 0.15, height = 0.2)
p_ChinaMain
print(P_HaiNan, vp = subvp)

#--------------Medthod2-----------------------------------------------------------------------------------
Width<-9
Height<-9
china_map2<-china_map2[!is.na(china_map2$long),]
china_map2<-china_map2[!is.na(china_map2$lat),]
china_map4<-china_map2
china_map4$long<-(china_map2$long-min(china_map2$long))/(max(china_map2$long)-min(china_map2$long))*Width+124
china_map4$lat<-(china_map2$lat-min(china_map2$lat))/(max(china_map2$lat)-min(china_map2$lat))*Height+16
china_map4$class<-rep("NanHai",nrow(china_map4))

china_map1$class<-rep("Mainland",nrow(china_map1))


NanHai2<- NanHai           #??ȡҵ??????
NanHai2$long<-(NanHai$long-min(china_map2$long))/(max(china_map2$long)-min(china_map2$long))*Width+124
NanHai2$lat<-(NanHai$lat-min(china_map2$lat))/(max(china_map2$lat)-min(china_map2$lat))*Height+16

china_map5<-rbind(china_map1,china_map4)

#china_data <- join(province_city, mydata, type="full")        #?ϲ?��?????ݿ?

ggplot()+
  geom_polygon(data=china_map5, aes(x=long, y=lat, group=interaction(class,group),fill=Value),colour="black",size=0.25)+ #?й???ͼ
  
  #geom_polygon(data=china_map4, aes(x=long, y=lat, group=group,fill=value), colour="black",size=0.25)+ #?й??Ϻ??Ŷ???
  geom_rect(aes(xmin=124, xmax=125+Width, ymin=15, ymax=16+Height),fill=NA,colour="black")+
  geom_line(data=NanHai2, aes(x=long, y=lat, group=ID), colour="black", size=1)+ #?й??Ϻ??Ŷ???
  
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11,'Spectral')))(32))+
  #geom_point(data=china_data, aes(x = jd,y = wd,size=zhibiao), fill="black", alpha=1,shape=21, colour="white")+ #????ͼ
  #scale_size_area(max_size=8)+      
  #coord_map("polyconic") +
  #guides(fill=guide_legend(title="value"))+
  ylim(15,55)+
  theme(
    #   #panel.grid = element_blank(),
    #   panel.background = element_blank(),
    #   axis.text = element_blank(),
    #   axis.ticks = element_blank(),
    #   axis.title = element_blank(),
    #   legend.direction="vertical",
    legend.position=c(0.2,0.2),
    legend.background = element_blank()
    #   legend.text.align=0
  )
