library(jpeg)
img<-readJPEG("luren.jpg")
imgDm<-dim(img)
imgRBG<-data.frame(
  x=rep(1:imgDm[2],each=imgDm[1]),
  y=rep(imgDm[1]:1,imgDm[2]),
  R=as.vector(img[,,1]),
  G=as.vector(img[,,2]),
  B=as.vector(img[,,3])
)
library(ggplot2)
plotTheme<-function(){
  theme(
    panel.background=element_rect(
      size=3,
      colour="black",
      fill="white"),
    axis.ticks=element_line(size=2),
    panel.grid.major=element_line(
      colour = "gray80",
    linetype = "dotted"),
    panel.grid.major=element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x=element_text(
      size = rel(1.2),
      face="bold"),
    axis.title.y=element_text(
      size = rel(1.2),
      face="bold"),
    plot.title=element_text(
      size = 20,
      face="bold",
      vjust=1.5)
  )
}
#plot the image
ggplot(data=imgRBG,aes(x=x,y=y))+
  geom_point(colour=rgb(imgRBG[c("R","G","B")]))+
  labs(title="Original Image: Colorful Bird")+
  xlab("x")+ylab("y")+plotTheme()
#K-means聚类
kClusters<-24
kMeans<-kmeans(imgRBG[,c("R","G","B")],centers = kClusters)
kColours<-rgb(kMeans$centers[kMeans$cluster,])
ggplot(data=imgRBG,aes(x=x,y=y))+
  geom_point(colour=kColours)+
  labs(title = paste("k-means Clustering of", kClusters, "Colours"))+
  xlab("x")+ylab("y")+plotTheme()
  