setwd("/Users/changmi-ra/Desktop/R-study")
getwd()

##12-1: 텍스트 마이닝, 형태소 분석
library(RColorBrewer)
library(KoNLP)
library(wordcloud)
library(dplyr)
pos=KoNLP::SimplePos09('아 정말 졸린 수업이다. Wow!!')
#품사 추출
library(stringr)
extracted=str_match(pos,'([가-힣]+)/[NP]')
extracted
keyword=extracted[,2]
keyword
keyword=keyword[!is.na(keyword)]
keyword


##12-2: 워드 클라우드
library(RColorBrewer)
library(KoNLP)
library(wordcloud)
library(dplyr)
useNIADic() #사전 설정하기
txt<-readLines("hiphop.txt") # 한글 깨짐 https://summerorange.tistory.com/44
head(txt) # 데이터 준비
library(stringr) #특수문자 제거
txt<-str_replace_all(txt,"\\W"," ")
nouns<-extractNoun(txt) #명사 추출
wordcount<-table(unlist(nouns)) #단어별 빈도표
df_word<-as.data.frame(wordcount,stringsAsFactors=F) #데이터 프레임
df_word<-rename(df_word, word=Var1, freq=Freq) #변수명 수정
df_word<-filter(df_word,nchar(word)>=2) #두글자 이상 단어 추출
top_20<-df_word%>%arrange(desc(freq))%>%head(20) #변화 확인 top 20
#워드 클라우드 만들기
library(wordcloud) #패키지 준비
library(RColorBrewer)
pal<-brewer.pal(8,"Dark2") #Dark2 색상 목록에서 8개 색상 추출
set.seed(1234) #난수 고정
wordcloud(words=df_word$word, #단어
          freq=df_word$freq, #빈도
          min.freq = 2, #최소 단어 빈도
          max.words = 200, #표현 단어 수
          random.order = F, # 고빈도 단어 중앙 배치
          rot.per = .1, #회전 단어 비율
          scale = c(4, 0.3), #단어 크기 범위
          colors = pal, # 색 목록
          family="AppleGothic") # 한글 깨짐 오류 해결


##12-3: 실습
#수상 전 기생충 기사
setwd("/Users/changmi-ra/Desktop/R-study")
getwd()
library(qgraph)
library(RColorBrewer)
library(wordcloud)
library(NLP)
library(tm)
library(stringr)
library(ggplot2)
library(KoNLP)
texts<-read.csv("news1_article.csv", fileEncoding = "UTF-8")

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

#수상 후 기생충 기사
getwd()
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

