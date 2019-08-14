# �����ĵ��洢λ��
# setwd("C:/Users/apple/Desktop/Textasdata")

library(rJava);  
library(Rwordseg); 
library(tm); 

# ��װ����TM��
#install.packages("C:\\SogouDownload\\tmcn_0.1-4.tar", repos=NULL, type="source") 
library(tmcn)
library(tm)
library(Rwordseg)
# lecture<-scan(file.choose(),sep="\n",what="",encoding="UTF-8")
# names(lecture)
# nchar(lecture)
# == �ı�Ԥ����  
# res=lecture[lecture!=" "]; 
# ls()
fix(res)

#�޳�URL  
# res=gsub(pattern="http:[a-zA-Z\\/\\.0-9]+","",res);   
# res=gsub(pattern="[n|t]","",res);   

#�޳������  
# res=gsub(pattern="[��|��|��|��|һ��|һ��|û��|����|����|ԭ��|����|����|��Ժ|���]","",res);

#�޳����� 
# res=gsub(pattern="/^[^0-9]*$/","",res); 

# ��װ�´ʵ�
#installDict("C:\\SogouDownload\\diming.scel","diming")
#installDict("C:\\SogouDownload\\xinli.scel","xinli")
#installDict("C:\\SogouDownload\\zzx.scel","zzx")
#listDict()
# uninstallDict()

# d.vec <- segmentCN("samgov.csv", returnType = "tm")

# read file after word segment, RĬ�������ı�ΪKGB���룬���ΪUTF-8����ʹ��encoding="UTF-8"
samgov.segment <- read.table("samgov.segment.csv", header = TRUE, fill = TRUE, stringsAsFactors = F, 
 sep = ",")
fix(samgov.segment)

# ����DTM�ļ�(document term matrix)
d.corpus <- Corpus(VectorSource(samgov.segment$content))
# fix(d.corpus )
inspect(d.corpus[1:20])
d.corpus <- tm_map(d.corpus, removeWords, stopwordsCN())
ctrl <- list(removePunctuation = TRUE, removeNumbers= TRUE,stopwords = stopwordsCN(), wordLengths = c(2, Inf))
d.dtm <- DocumentTermMatrix(d.corpus, control = ctrl)
inspect(d.dtm[1:10, 110:112])
fix(d.dtm)

# ��Ƶ����
findFreqTerms(d.dtm,100)
findFreqTerms(d.dtm,50)

# ��Ƶ���Թ�ϵ
findAssocs(d.dtm, "���", 0.5)
findAssocs(d.dtm, "����", 0.5)

# ɾ��ϡ�����
d.dtm.sub <- removeSparseTerms(d.dtm, 0.99)
dim(d.dtm.sub)
dim(d.dtm)
findAssocs(d.dtm.sub, "ũ��", 0.5)

# �������
library(proxy) # proxy�е�dist�������Լ����ĵ�����������ƶȣ���Ϊ��������Ļ���
d.dist <- proxy:: dist(as.matrix(d.dtm),method='cosine')
heatmap(as.matrix(d.dist),labRow=FALSE, labCol=FALSE)
d.clust <- hclust(d.dist) #�������
result<-cutree(d.clust,k=5)
summary(result)
result
plot(d.clust)

# �������
library(topicmodels)
ctm<-CTM(d.dtm,k=5, control=list(seed=111))
Terms <- terms(ctm, 10)
Terms[,1:5]

ctm<-CTM(d.dtm,k=10, control=list(seed=1234))
Terms <- terms(ctm, 20)
Terms[,1:10]