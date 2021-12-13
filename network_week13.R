##13-3 : 실습
setwd("/Users/changmi-ra/Desktop/R-study")
library(qgraph)
library(RColorBrewer)
library(wordcloud)
library(NLP)
library(tm)
library(stringr)
library(ggplot2)
library(KoNLP)
texts<-read.csv("news2_article.csv", fileEncoding = "UTF-8")

ko.words=function(texts){
  d=str_split(texts,";")
  extracted=tolower(str_match(d,'([가-힣a-zA-Z]+)/[NVO]'))
  keyword=extracted[,2]
  keyword[!is.na(keyword)]
}

cps=Corpus(VectorSource(texts$article_POS))
tdm<-
  TermDocumentMatrix(cps,control=list(tokenize=ko.words,
                                      removePunctuation=T,
                                      removeNumbers=T,
                                      worldLengths=c(2,Inf)))
Encoding(tdm$dimnames$Terms)="CP949" 

dtm=as.DocumentTermMatrix(tdm)                     
freq=sort(colSums(as.matrix(dtm)), decreasing = TRUE)
wf=data.frame(word=names(freq), freq=freq)

p=ggplot(subset(wf,freq>50), aes(word, freq))
p=p+geom_bar(stat="identity")
p=p+theme(axis.text.x = element_text(angle=90, hjust = 1))
p
wordcloud(names(freq), freq, min.freq = 100, color = brewer.pal(6,"Dark2"),family="AppleGothic")

# 한글 처리된 tdm으로 단어 개수 계산, 그림그릴 준비
dtm = as.DocumentTermMatrix(tdm)

freq=sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wf=data.frame(word=names(freq), freq=freq)

p=ggplot(subset(wf, freq>50), aes(word, freq))
p=p+geom_bar(stat="identity")
p=p+theme(axis.text.x = element_text(angle=90, hjust=1))
p
wordcloud(names(freq), freq, min.freq=100, color=brewer.pal(6, "Dark2"), family="AppleGothic")

# 매트릭스 데이터 만들기
tdm.matrix=as.matrix(tdm)
word.count=rowSums(tdm.matrix)
word.order=order(word.count,decreasing=T)
freq.word=tdm.matrix[word.order[1:50],]
rownames(tdm.matrix)[word.order[1:50]] 

#최종 매트릭스 
co.matrix = freq.word %*% t(freq.word)
co.matrix
data<-co.matrix
data

#네트워크 그리기
qg <- qgraph(co.matrix,
             labels=rownames(co.matrix),
             diag=F,
             layout='spring',
             edge.color='black',
             vsize = log(diag(co.matrix))*1.5)

par(family="AppleGothic") # 한글 깨짐 오류
plot(qg)










