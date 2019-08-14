#This is a practice of RCurl

#basic knowledge 
######################################################################################################
#basic format scheme://host[:port#]/path/.../[??query-string][#anchor]
#scheme      eg:http,https,ftp
#host HTTP server's IP address and domin name
#port#  path to seek source
#query-string data which will be sent to http server
#anchor- "anchor"

#about request
#METHOD/path-to-resource HTTP/Version-number
#Header-Name-1:value
#Header-Name-2:value
#............
#Optional request body

#here we always have
#Method is the means of request eg GET POST HEAD PUT
#Path-to resource means requested source
#Http/Version-number is Version number of HTTP protocol

#Host
#Accept
#Accept-encoding
#Accept-language
#User-agent
#Cookie
#Refer
#Connection

#Http/cersion-number      status code     message
#Header-Name-1:value
#Header-Name-2:value

#Optional Response body
#HTTP/vesion-number show the version number of Http
#status-conde and message 

#HTTP/1.1 define five status code 
#-1XX tip- received successfully and continue to do
#-2XX successfully- received understand and take in
#-3XX reorietation
#-4XX client-side error
#-5XX server error
##########################################################################################################

#RCurl's three function getURL() getForm() postForm()
#######################################################################################################
library(RCurl)
url.exists("http://www.baidu.com")
url.exists("ftp.djdb.do")
d=debugGatherer()
temp<-getURL("http://www.dataguru.cn/",debugfunction=d$update,verbose=TRUE)
#return server's received header
cat(d$value()[3])
#return server address and port
cat(d$value()[1])
#return server side header
cat(d$value()[2])

#getURL() function information
#string form 
headers=basicTextGatherer()
txt=getURL("http://www.dataguru.cn/",headerfunction=headers$update)
names(headers$value())
headers$value()

#table form
h=basicHeaderGatherer()
txtt=getURL("http://www.dataguru.cn/",headerfunction=h$update)
names(h$value())
h$value()

#search information of curl  ????
#??????һ????????????ָ?? ????һ??Ӧ?ó???Ҫ????????ϵͳ???????ݿ⡢????ϵͳ???????????ڴ?????????ʱ??
#??Ҫʹ?þ???
curl = getCurlHandle()
d=getURL("http://www.dataguru.cn/", curl = curl)
getCurlInfo(curl)$response.code
getCurlInfo(curl)

#pretend  useragent 
myheader<-c(
  "User-Agent"="Mozilla/5.0 (iPhone; U; CPU iPhone OS 4_3_3 like Mac OS X; en-us) AppleWebKit/533.17.9 (KHTML, like Gecko) Version/5.0.2 Mobile/8J2 Safari/6533.18.5",
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language"="en-us",
  "Connection"="keep-alive",
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)
d=debugGatherer()
temp1<-getURL("http://www.baidu.com",httpheader=myheader,debugfunction=d$update,verbose=FALSE)
cat(d$value()[3])

#https://www.baidu.com/s?ie=utf-8&f=8&rsv_bp=1&tn=54002054_dg&
#  wd=r????&oq=R&rsv_pq=e64e5984000280d1&
#  rsv_t=c55czyNPaXM3s%2F%2BxjWgMURx5zWvHrN2PC1ZATMlR7zOZVnOKlhfLNeC7qPBizMQbRW4&
#  rqlang=cn&rsv_enter=1&inputT=520&rsv_sug3=49&rsv_sug1=25&rsv_sug7=100&bs=R

char<-c("https://www.baidu.com/baidu?wd=R????&tn=54002054_dg&ie=utf-8")
getFormParams(char)
#ucc<-getForm("http://www.baidu.com/s","wd"="R????",rsv_sug)
#ucc<-getForm("http://www.baidu.com/s","wd"="R????",ie="utf-8",f="8",rsv_bp="1",tm="54002054_dg",oq="R",
#             rsv_pq="e64e5984000280d1",
#             rsv_t="c55czyNPaXM3s%2F%2BxjWgMURx5zWvHrN2PC1ZATMlR7zOZVnOKlhfLNeC7qPBizMQbRW4",
#             rqlang="cn",rsv_enter="1",inputT="520",rsv_sug3="49",rsv_sug1="25",rsv_sug7="100",bs="R")
ucc<-getForm("http://www.baidu.com/s","wd"="R????",ie="utf-8",tn="54002054_dg")
write.table(ucc,"url.txt")
###########################################################################################################

#download some files and practices
#####################################################################################################
#Curl's partial parameter's setting
#verbose?????????ʵĽ?????Ϣ
#httpheader?????÷?????Ϣ??ͷ
#encoding=??UTF-8????GBK??
#debugfunction,headerfunction,curl
#params???ύ?Ĳ?????
#dirlistonly??????ȡĿ¼
#ftp.wcc.nrcs.usda.gov/data/snow/snow_course/table/history/idaho/
#followlocation??֧???ض???
#http://www.sina.com
#maxredirs???????ض???????

######################################################################################################
#use getBinaryURL to download files
temp<-getBinaryURL(url)
note <-file("hellodata.xls", open = "wb")
writeBin(temp,note)
close(note)

url<-"ftp.wcc.nrcs.usda.gov/data/snow/snow_course/table/history/idaho/"
filename<-getURL(url,dirlistonly=TRUE)
#filename
url1<-"http://www.baidu.com/"
curl=getCurlHandle()
destination=getURL("http://www.sina.com",curl = curl)
getCurlInfo(curl)$response.code
temp<-getBinaryURL(url1)
note <-file("hellodata.xls", open = "wb")
writeBin(temp,note)
close(note)

#Split the elements of a character vector x into substrings according to 
#the matches to substring split within them.
strsplit()
class(Sys.time())
#Given a list structure x, unlist simplifies it to 
#produce a vector which contains all the atomic components which occur in x.
unlist(strsplit(as.character(Sys.time()),""))

#use getBinaryURL() to do some practices
library(stringr)
Rhtml<-"ftp.wcc.nrcs.usda.gov/data/snow/snow_course/table/history/idaho/"
tem<-getURL(Rhtml,dirlistonly=TRUE)

#Example down a table
url<-"http://www.bioguo.org/AnimalTFDB/BrowseAllTF.php?spe=Mus_musculus"
wp<-getURL(url)
doc <-htmlParse(wp, asText= TRUE)
tables <-readHTMLTable(doc)

curl=getCurlHandle()
destination<-getURL("http://www.sina.com",curl = curl,followloacation=TRUE,maxredirs=5)
getCurlInfo(curl)$response.code
tecc<-unlist(strsplit(tem,"<li><a href=\""))
files=strsplit(tecc,"\"")
files=unlist(lapply(files, function(x1){x1[1]}))
files<-files[-(1:2)]

base<-"http://rfunction.com/code/1202/"
for(i in 1:length(files)){
  url=str_c(base,files[i])
  tems1<-getBinaryURL(url)
  note<-file("download/hellodata.xlsx",open = "wb")
  writeBin(tems1,note)
  close(note)
  download.file(url,destfile = paste("download/",files[i],sep=''))
  print(paste("??",i,"???Ѿ?????",sep=''))
}

#use getBinaryURL() to download files_2
library(stringr)
curl=getCurlHandle()
Rhtml<-"http://rfunction.com/code/1202/"
tem<-getURL(Rhtml,curl=curl)
tecc<-unlist(strsplit(tem,"<li><a href=\""))
files=strsplit(tecc,"\"")
files=unlist(lapply(files, function(x1){x1[1]}))
files<-files[-(1:2)]

base<-"http://rfunction.com/code/1202/"
for(i in 1:length(files)){
  url=str_c(base,files[i])
  tems1<-getBinaryURL(url)
  note<-file("download/hellodata.xlsx",open = "wb")
  writeBin(tems1,note)
  close(note)
  download.file(url,destfile = paste("download/",files[i],sep=''))
  print(paste("??",i,"???Ѿ?????",sep=''))
}

#Example ץȡ????ί????????ҵ????
#http://www.sasac.gov.cn/n2588035/n2641579/n2641645/index.html
library(XML)
url<-"http://www.sasac.gov.cn/n2588035/n2641579/n2641645/index.html"
wp <- getURL(url,.encoding = "UTF-8")
doc <- htmlParse(wp, asText = TRUE)
tables <- readHTMLTable(doc,which = 3)

#########################################################################################################
# Xpath 
# /????ʾѡ?????ڵ?
# ???//????ʾѡ??????λ?õ?ĳ???ڵ?
# ???@????ʾѡ??ĳ??????
# ???*??ʾƥ???κ?Ԫ?ؽڵ㡣
# ???@*??ʾƥ???κ?????ֵ??
# ???node()??ʾƥ???κ????͵Ľڵ㡣

##########################################################################################################

#Example 
# http://www.w3school.com.cn/example/xmle/books.xml
url1<-"http://www.w3school.com.cn/example/xmle/books.xml"
doc<-xmlParse(url1)
getNodeSet(doc,'/bookstore/book[1]')
getNodeSet(doc,'/bookstore/book[last()]')
getNodeSet(doc,'/bookstore/book[position()<3]')
getNodeSet(doc,'//title/@lang')
getNodeSet(doc,"//book/title | //book/price")

#Example 
# http://t.dianping.com/list/guangzhou?q=??Ӱ
url<-"http://t.dianping.com/list/beijing?q=??ʳ"
myheader<-c("User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
            "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
            "Accept-Language"="en-us",
            "Connection"="keep-alive",
            "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
)
temp2<-getURL(url,encoding = "UTF-8",httpheader=myheader)
seek2<-htmlParse(temp2)
getNodeSet(seek1,'//body/div[2]/div/h3')
getNodeSet(seek1,'//a[@target="_blank"]')
getNodeSet(seek1,'//h3')
#########################################################################################################
#????????ʽ??ʶ
#???ַ?????????һ???߼???ʽ
#??ҪӦ?ö??????ı?
#???ã?1???߼????? 2????׼ץȡ
#????????ʽ???ص??ǣ?
# 1 ?????ԡ??߼??Ժ͹????Էǳ???ǿ
# 2 ????Ѹ?ٵ??ü??򵥵ķ?ʽ?ﵽ?ַ????ĸ??ӿ???
# 3 ???ڸսӴ?????��˵???Ƚϻ?ɬ?Ѷ?

# \??ת???ַ?
# ???.?????˻??????????????ַ?
# ???^??һ???ַ???????ʼ
# ???$??һ???ַ????Ľ???
# ???*?????????߶???֮ǰ???ַ?
# ???+??һ?????߶???֮ǰ???ַ?
# ??????????????һ??֮ǰ???ַ?
#????????ַ?????Ҫת???ַ?\��ת????ʾ

# ??��??[]??????????ƥ???????κ?һ???ַ?????^??[]?д??���?ǡ?, -???���֮?䡱
# -[qjk]??q??j??k??????һ???ַ?
# -[^qjk]????q??j??k???????????ַ?
# -[a-z]??a??z??????һ??Сд?ַ?
# -[^a-z]?? ??????һ??a??zСд?ַ????????ַ????????Ǵ?д?ַ???
# -[a-zA-Z]??????һ??Ӣ????ĸ
# -[a-z]+?? һ?????߶???СдӢ????ĸ
# -|??????
# -С��??()?뻨��?ţ??????ϡ?|??ʹ??

# ???õ?????ת???ַ?????
# \n?????з?
# \t??tab
# \w????????ĸ????��?»??ߣ?????????	?? [a-zA-Z0-9_]
# \W??\w?ķ??? ??[^a-zA-Z0-9_]
# \d??????һ?????? ??[0-9]
# \D??\d?ķ??? ??[^0-9]
# \s??????һ???ո񣬱???space, tab, newline ??
# \S??\s?ķ??壬????һ???ǿո?

#########################################################################################################
#Example
pattern<-"^[A-Za-z0-9\\._%+-]+@[A-Za-z0-9\\.-]+\\.[A-Za-z]{2,4}$"
#pattern could express a address of E-mail such as sunshine@163.com
#\\. is . ;_%+-means this expression include _ or % or + or - ;
# grepl??????һ???߼?ֵ
# grep??????ƥ????id

# ?????滻??sub??gsub

# regexpr??????һ?????֣?1??ʾƥ?䣬-1??ʾ??ƥ?䣬?Լ?��?????ԣ?ƥ???ĳ??Ⱥ? ?Ƿ?ʹ??useBytes
# regexec??????һ??list???ַ????е?һ??ƥ?估?䳤???Լ??Ƿ?ʹ??useBytes
# gregexpr??????һ??list?? ÿһ??ƥ?估?䳤???Լ??Ƿ?ʹ??useBytes

list1<-c("shunshine.163.com","huijie","2834124589@qq.com","nate_yupq@outlook.com","kjhfdh@ji.jikei")
list2<-c("shunshine.@163.com","huijie","2834124589@qq.com","nate_yupq@outlook.com","kjhfdh@ji.jikei")

# http://finance.sina.com.cn/realstock/company/sh601068/nc.shtml
# http://stock.finance.sina.com.cn/hkstock/quotes/02068.html

url2<-"http://stock.finance.sina.com.cn/hkstock/quotes/02068.html"
temp2<-getURL(url2,.encoding = "UTF-8")
kk<-strsplit(temp2,"\r\n")[[1]]

########################################################################################################