# 导入程序包
# install.packages("readxl")
library(readxl)
# install.packages("jiebaR")
# install.packages("plyr")
# install.packages("wordcloud2")
# install.packages("Rcpp")

library(jiebaR)
library(plyr)
library(wordcloud2)

# 自助读入数据
gov <- read_excel(file.choose())
head(gov)
str(gov)
View(gov)

# 使用jiebaR分词
### 设置分词引擎
wk = worker()

### 了解jieba的工作原理
wk
wk["江西省乐村庄联名请求解决失地农民就业和生活保障的报告"]
wk<='江西省乐村庄联名请求解决失地农民就业和生活保障的报告'
segment( "江西省乐村庄联名请求解决失地农民就业和生活保障的报告" , wk )

### 词性标注
text <- '江西省乐村庄联名请求解决失地农民就业和生活保障的报告'
tagseg <- worker('tag')
tagseg[text]

### 配置词典
show_dictpath()
dir(show_dictpath())
scan(file="C:/Users/Max Meng/Documents/R/win-library/3.3/jiebaRD/dict/jieba.dict.utf8",
          what=character(),nlines=50,sep='\n',
           encoding='utf-8',fileEncoding='utf-8')
scan(file="C:/Users/Max Meng/Documents/R/win-library/3.3/jiebaRD/dict/user.dict.utf8",
     what=character(),nlines=50,sep='\n',
     encoding='utf-8',fileEncoding='utf-8')
### 自定义用户词典
# 在jiebaR分词引擎中添加自定义词汇
# 例如，不添加自定义词汇
sentence <- '尊敬的苏书记您好，希望您在百忙之中能关注此事，我们是弱势群体，得了尿毒症，靠血液透析维持生命，家里为我们治病花了好多的钱，已经到了山穷水尽的地步。前些日子听说好多病友的了丙肝，自己也不例外。我们只求生存的希望，可现在尿毒症加丙肝，我们的健康权，生存权，知情权受到伤害。现在连生活的信心也没有，这事我们已向医院领导反映过，到现在也没有结果。院方找病人个别说服签订1一次性赔偿一万元，2在本院治疗丙肝，3免费做血透。不签不治疗，到现在还没对病人实施治疗。丙肝给病人带来的痛苦是肝硬化，肝癌，肝病发作一万元只是杯水车薪，现在党和政府关爱尿毒病人可以免费做血透，但很多人会因为肝病去命。现在好多病友还不知道自己得了丙肝，医院前年可能就知道到现在还一直隐瞒，给病人造成了严重的后果。给家庭造成了严重的负担，给社会造成不良影响，人性何在，医德何在，道德何在。希望您呢救我们于水深火热之中，还我们健康权。'
segment(sentence, wk)
# 添加自定义词汇
new_user_word(wk, '百忙之中')
segment(sentence, wk)

### 读入自定义停词表
wk<-worker(stop_word="C:/text/homework/chineseStopWords.txt")

### 分词和词频统计
segment<-wk["书记，您好！真的很抱歉，您日理万机，还要打搅您！我是南昌市安义县的一位普通市民。开学前夕，在外地务工的叔叔将小孩带回老家，打算就读安义中学高中部。可是，教育局长涂赞宝为了将部分学生赶往教学质量低劣的职业高中就读，竟强制安义中学不准招收县内初中毕业生，无奈，学生家长失去自主选择的权利，只好将小孩背井离乡安排到外地就读，许多小孩从小跟随父母四处流浪，而现在面临继续漂泊。而涂赞宝作为教育主管部门领导，擅自利用手中职权，签字批准六十六名科级以上领导干部子女及其本人关系户子女进入安义中学就读，这事在全县群众当中引起极大反响！（情况绝对属实，部分接收学生名单复印件留安义中学，可以调查）现在的政府提倡建设和谐社会，这样的教育部门如何让市民感受到和谐？这样的领导干部如何能创先争优，让人民得到实惠啊！"]
freq(segment)

### 统计词频并自动提取关键词，TF-IDF
## TF-IDF = TF(词频) * 逆文档频率(IDF)，TF（Term Frequency）代表词频，
## IDF（Inverse Document Frequency）表示逆文档频率。如果某个词在文章中多次出现，
## 而且不是停止词，那么它很可能就反应了这段文章的特性，这就是我们要找的关键词。
## 再通过IDF来算出每个词的权重，不常见的词出现的频率越高，则权重越大。
scan(file="C:/Users/Max Meng/Documents/R/win-library/3.3/jiebaRD/dict/idf.utf8",
      what=character(),nlines=50,sep='\n',
      encoding='utf-8',fileEncoding='utf-8')
keys = worker("keywords",topn=5)
keys <=text
vector_keywords(segment,keys)
keys <="./government.csv"

# 对每条记录进行分词
segwords <- sapply(gov$content, segment, wk)
head(segwords)
# fix(segwords)
                     
# 制作词云
# 计算词频
# 加载词云分析模块
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

# 绘制词云
wordcloud2(wf[1:100,])
wordcloud2(wf[1:200,], backgroundColor = 'green')

# 删除单个字符的词制作词云,自定义函数：保留至少2个字符长度的词语
more2words <- function(x){
  words = c()
  for (word in x) {
    if (nchar(word)>1) words = c(words,word)
  }
  return(words)
}

segwords2 <- more2words(unlist(segwords))

# 计算词频
wf2 <- unlist(segwords2)
wf2 <- as.data.frame(table(wf2))
wf2 <- arrange(wf2, desc(Freq))
head(wf2)  

# 映射windows字体
windowsFonts(A=windowsFont("微软雅黑"))
par(family="A") 
wordcloud(wf2$wf2,wf2$Freq,random.order=FALSE,rot.per=.45,col=brewer.pal(9,"Set1"))
wordcloud(wf2$wf2,wf2$Freq,random.order=FALSE,rot.per=.45,col=rainbow(length(wf2$Freq)))
wordcloud(wf2$wf2,wf2$Freq,random.order=FALSE,rot.per=.45,col=rainbow(length(wf2$Freq)),max.words=100)
wordcloud(wf2$wf2,wf2$Freq,random.order=FALSE,min.freq=100, rot.per=.45,col=rainbow(length(wf2$Freq)),max.words=100)

wordcloud2(wf2[1:100,], backgroundColor = 'red')

# 由于该评论就是针对政府反映意见，删掉词云中的常用词“政府，书记”等。
View(wf2)
wordcloud2(wf2[4:103,], backgroundColor = 'green')

# 输出词频表 
write.csv(wf2, file="governmentword.csv", row.names=FALSE) 