download.file(url, destfile, method, quiet = FALSE, mode = "w",
              cacheOK = TRUE,
              extra = getOption("download.file.extra"))

library(rvest)
url<-"http://download.dogwood.com.cn/online/gfxz2018/index.html"
page<-read_html(url,encoding="UTF-8")
durl<-page%>%html_nodes('ul div.info a')%>%html_attrs()  

durl2<-list()
for(i in 1:130)
  durl2[i]<-durl[i*2]

durl3<-c(1:length(durl2))  #初始化一个和durl2长度相等的durl3 
for(i in 1:length(durl2))  
  durl3[i]<-durl2[[i]][1]   
durl3 <-durl3[-c(129:130)] #删除最后两个链接，因为那是两个视频，我不想下载

name<-page%>%html_nodes('ul div.info span')%>%html_text()
name<-name[-c(129:130)] #在这里也删除最后两个视频的名称

name2<-paste(name,'.mp3')


for(i in 1:length(name2))
  download.file(url=durl3[i],destfile=name2[i])