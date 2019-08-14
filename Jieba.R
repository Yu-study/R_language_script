# ��������
# install.packages("readxl")
library(readxl)
# install.packages("jiebaR")
# install.packages("plyr")
# install.packages("wordcloud2")
# install.packages("Rcpp")

library(jiebaR)
library(plyr)
library(wordcloud2)

# ������������
gov <- read_excel(file.choose())
head(gov)
str(gov)
View(gov)

# ʹ��jiebaR�ִ�
### ���÷ִ�����
wk = worker()

### �˽�jieba�Ĺ���ԭ��
wk
wk["����ʡ�ִ�ׯ����������ʧ��ũ���ҵ������ϵı���"]
wk<='����ʡ�ִ�ׯ����������ʧ��ũ���ҵ������ϵı���'
segment( "����ʡ�ִ�ׯ����������ʧ��ũ���ҵ������ϵı���" , wk )

### ���Ա�ע
text <- '����ʡ�ִ�ׯ����������ʧ��ũ���ҵ������ϵı���'
tagseg <- worker('tag')
tagseg[text]

### ���ôʵ�
show_dictpath()
dir(show_dictpath())
scan(file="C:/Users/Max Meng/Documents/R/win-library/3.3/jiebaRD/dict/jieba.dict.utf8",
          what=character(),nlines=50,sep='\n',
           encoding='utf-8',fileEncoding='utf-8')
scan(file="C:/Users/Max Meng/Documents/R/win-library/3.3/jiebaRD/dict/user.dict.utf8",
     what=character(),nlines=50,sep='\n',
     encoding='utf-8',fileEncoding='utf-8')
### �Զ����û��ʵ�
# ��jiebaR�ִ������������Զ���ʻ�
# ���磬�������Զ���ʻ�
sentence <- '�𾴵���������ã�ϣ�����ڰ�æ֮���ܹ�ע���£�����������Ⱥ�壬������֢����ѪҺ͸��ά������������Ϊ�����β����˺ö��Ǯ���Ѿ�����ɽ��ˮ���ĵز���ǰЩ������˵�öಡ�ѵ��˱��Σ��Լ�Ҳ�����⡣����ֻ�������ϣ������������֢�ӱ��Σ����ǵĽ���Ȩ������Ȩ��֪��Ȩ�ܵ��˺������������������Ҳû�У�������������ҽԺ�쵼��ӳ����������Ҳû�н����Ժ���Ҳ��˸���˵��ǩ��1һ�����⳥һ��Ԫ��2�ڱ�Ժ���Ʊ��Σ�3�����Ѫ͸����ǩ�����ƣ������ڻ�û�Բ���ʵʩ���ơ����θ����˴�����ʹ���Ǹ�Ӳ�����ΰ����β�����һ��Ԫֻ�Ǳ�ˮ��н�����ڵ��������ذ��򶾲��˿��������Ѫ͸�����ܶ��˻���Ϊ�β�ȥ�������ںöಡ�ѻ���֪���Լ����˱��Σ�ҽԺǰ����ܾ�֪�������ڻ�һֱ��������������������صĺ��������ͥ��������صĸ������������ɲ���Ӱ�죬���Ժ��ڣ�ҽ�º��ڣ����º��ڡ�ϣ�����ؾ�������ˮ�����֮�У������ǽ���Ȩ��'
segment(sentence, wk)
# �����Զ���ʻ�
new_user_word(wk, '��æ֮��')
segment(sentence, wk)

### �����Զ���ͣ�ʱ�
wk<-worker(stop_word="C:/text/homework/chineseStopWords.txt")

### �ִʺʹ�Ƶͳ��
segment<-wk["��ǣ����ã���ĺܱ�Ǹ���������������Ҫ������������ϲ��а����ص�һλ��ͨ���񡣿�ѧǰϦ��������񹤵����彫С�������ϼң�����Ͷ�������ѧ���в������ǣ������ֳ�Ϳ�ޱ�Ϊ�˽�����ѧ��������ѧ�������ӵ�ְҵ���оͶ�����ǿ�ư�����ѧ��׼�������ڳ��б�ҵ�������Σ�ѧ���ҳ�ʧȥ����ѡ���Ȩ����ֻ�ý�С���������簲�ŵ���ؾͶ�������С����С���游ĸ�Ĵ����ˣ����������ټ���Ư������Ϳ�ޱ���Ϊ�������ܲ����쵼��������������ְȨ��ǩ����׼��ʮ�����Ƽ������쵼�ɲ���Ů���䱾�˹�ϵ����Ů���밲����ѧ�Ͷ���������ȫ��Ⱥ�ڵ������𼫴��죡�����������ʵ�����ֽ���ѧ��������ӡ����������ѧ�����Ե��飩���ڵ������ᳫ�����г��ᣬ�����Ľ������������������ܵ���г���������쵼�ɲ�����ܴ������ţ�������õ�ʵ�ݰ���"]
freq(segment)

### ͳ�ƴ�Ƶ���Զ���ȡ�ؼ��ʣ�TF-IDF
## TF-IDF = TF(��Ƶ) * ���ĵ�Ƶ��(IDF)��TF��Term Frequency��������Ƶ��
## IDF��Inverse Document Frequency����ʾ���ĵ�Ƶ�ʡ����ĳ�����������ж�γ��֣�
## ���Ҳ���ֹͣ�ʣ���ô���ܿ��ܾͷ�Ӧ��������µ����ԣ����������Ҫ�ҵĹؼ��ʡ�
## ��ͨ��IDF�����ÿ���ʵ�Ȩ�أ��������Ĵʳ��ֵ�Ƶ��Խ�ߣ���Ȩ��Խ��
scan(file="C:/Users/Max Meng/Documents/R/win-library/3.3/jiebaRD/dict/idf.utf8",
      what=character(),nlines=50,sep='\n',
      encoding='utf-8',fileEncoding='utf-8')
keys = worker("keywords",topn=5)
keys <=text
vector_keywords(segment,keys)
keys <="./government.csv"

# ��ÿ����¼���зִ�
segwords <- sapply(gov$content, segment, wk)
head(segwords)
# fix(segwords)
                     
# ��������
# �����Ƶ
# ���ش��Ʒ���ģ��
library(Rcpp)
# install.packages("wordcloud")
library(wordcloud)
library(wordcloud2)
require(scales)
library(RColorBrewer)
wf <- unlist(segwords)
wf <- as.data.frame(table(wf))
wf <- arrange(wf, desc(Freq))
head(wf)  
View(wf)

# ���ƴ���
wordcloud2(wf[1:100,])
wordcloud2(wf[1:200,], backgroundColor = 'green')

# ɾ�������ַ��Ĵ���������,�Զ��庯������������2���ַ����ȵĴ���
more2words <- function(x){
  words = c()
  for (word in x) {
    if (nchar(word)>1) words = c(words,word)
  }
  return(words)
}

segwords2 <- more2words(unlist(segwords))

# �����Ƶ
wf2 <- unlist(segwords2)
wf2 <- as.data.frame(table(wf2))
wf2 <- arrange(wf2, desc(Freq))
head(wf2)  

# ӳ��windows����
windowsFonts(A=windowsFont("΢���ź�"))
par(family="A") 
wordcloud(wf2$wf2,wf2$Freq,random.order=FALSE,rot.per=.45,col=brewer.pal(9,"Set1"))
wordcloud(wf2$wf2,wf2$Freq,random.order=FALSE,rot.per=.45,col=rainbow(length(wf2$Freq)))
wordcloud(wf2$wf2,wf2$Freq,random.order=FALSE,rot.per=.45,col=rainbow(length(wf2$Freq)),max.words=100)
wordcloud(wf2$wf2,wf2$Freq,random.order=FALSE,min.freq=100, rot.per=.45,col=rainbow(length(wf2$Freq)),max.words=100)

wordcloud2(wf2[1:100,], backgroundColor = 'red')

# ���ڸ����۾������������ӳ�����ɾ�������еĳ��ôʡ���������ǡ��ȡ�
View(wf2)
wordcloud2(wf2[4:103,], backgroundColor = 'green')

# �����Ƶ�� 
write.csv(wf2, file="governmentword.csv", row.names=FALSE) 