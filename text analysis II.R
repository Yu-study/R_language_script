# 设置文档存储位置
# setwd("C:/Users/apple/Desktop/Textasdata")

library(rJava);  
library(Rwordseg); 
library(tm); 

# 安装中文TM包
#install.packages("C:\\SogouDownload\\tmcn_0.1-4.tar", repos=NULL, type="source") 
library(tmcn)
library(tm)
library(Rwordseg)
# lecture<-scan(file.choose(),sep="\n",what="",encoding="UTF-8")
# names(lecture)
# nchar(lecture)
# == 文本预处理  
# res=lecture[lecture!=" "]; 
# ls()
fix(res)

#剔除URL  
# res=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",res);   
# res=gsub(pattern="[n|t]","",res);   

#剔除特殊词  
# res=gsub(pattern="[我|你|的|了|一下|一个|没有|这样|现在|原告|被告|北京|法院|简称]","",res);

#剔除数字 
# res=gsub(pattern="/^[^0-9]*$/","",res); 

# 安装新词典
#installDict("C:\\SogouDownload\\diming.scel","diming")
#installDict("C:\\SogouDownload\\xinli.scel","xinli")
#installDict("C:\\SogouDownload\\zzx.scel","zzx")
#listDict()
# uninstallDict()

# d.vec <- segmentCN("samgov.csv", returnType = "tm")

# read file after word segment, R默认中文文本为KGB编码，如果为UTF-8，则使用encoding="UTF-8"
samgov.segment <- read.table("samgov.segment.csv", header = TRUE, fill = TRUE, stringsAsFactors = F, 
 sep = ",")
fix(samgov.segment)

# 创建DTM文件(document term matrix)
d.corpus <- Corpus(VectorSource(samgov.segment$content))
# fix(d.corpus )
inspect(d.corpus[1:20])
d.corpus <- tm_map(d.corpus, removeWords, stopwordsCN())
ctrl <- list(removePunctuation = TRUE, removeNumbers= TRUE,stopwords = stopwordsCN(), wordLengths = c(2, Inf))
d.dtm <- DocumentTermMatrix(d.corpus, control = ctrl)
inspect(d.dtm[1:10, 110:112])
fix(d.dtm)

# 词频分析
findFreqTerms(d.dtm,100)
findFreqTerms(d.dtm,50)

# 词频共显关系
findAssocs(d.dtm, "社会", 0.5)
findAssocs(d.dtm, "征用", 0.5)

# 删除稀疏矩阵
d.dtm.sub <- removeSparseTerms(d.dtm, 0.99)
dim(d.dtm.sub)
dim(d.dtm)
findAssocs(d.dtm.sub, "农民", 0.5)

# 聚类分析
library(proxy) # proxy中的dist函数可以计算文档间的余弦相似度，作为聚类分析的基础
d.dist <- proxy:: dist(as.matrix(d.dtm),method='cosine')
heatmap(as.matrix(d.dist),labRow=FALSE, labCol=FALSE)
d.clust <- hclust(d.dist) #聚类分析
result<-cutree(d.clust,k=5)
summary(result)
result
plot(d.clust)

# 主题分析
library(topicmodels)
ctm<-CTM(d.dtm,k=5, control=list(seed=111))
Terms <- terms(ctm, 10)
Terms[,1:5]

ctm<-CTM(d.dtm,k=10, control=list(seed=1234))
Terms <- terms(ctm, 20)
Terms[,1:10]
