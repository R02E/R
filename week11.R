##11-1 : 웹데이터 수집
setwd("/Users/changmi-ra/Desktop/R-study")
getwd()
library(RCurl)
library(XML)
searchUrl<-"https://openapi.naver.com/v1/search/news.xml"
Client_ID<- "X6znFBINSIDvY1ZnQfRg"
Client_Secret<-"y2lmSkRJYA"
query<-URLencode(iconv("BTS", "euc-kr", "UTF-8"))
url<-paste(searchUrl, "?query=", query, "&display=20", sep="")
doc<-getURL(url, 
            httpheader=c('Content-Type'="application/xml",
                         'X-Naver-Client-Id'=Client_ID,
                         'X-Naver-Client-Secret'=Client_Secret ))
doc2<-htmlParse(doc, encoding = "UTF-8")
text<-xpathSApply(doc2, "//item/description", xmlValue)
text


##11-3 : 실습
#본인 이름으로 뉴스 검색
library(httr)
library(rvest)
library(dplyr)
library(stringr)
url='https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EC%9E%A5%EB%AF%B8%EB%9D%BC&sort=0&photo=0&field=0&pd=3&ds=2018.11.16&de=2021.11.16&cluster_rank=221&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:from20181116to20211116,a:all&start='
naver_url_list<-c()
naver_url_list
for(i in 1:36)
  {i=(i*10)-9
  naver_url=paste(url,i,sep='')
  naver_url_list<-c(naver_url_list,naver_url)}
head(naver_url_list,5)
news_url<-c()
for(i in 1:length(naver_url_list)){
  naver_url<-naver_url_list[i]
  html<-read_html(naver_url)
  temp<-unique(html_nodes(html,'.info_group')%>%
                 html_nodes('a')%>%
                 html_attr('href'))
  news_url<-c(news_url,temp)
  print(i)
  }
View(news_url)
#네이버 뉴스만 추출, 스포츠뉴스 제거
df_news_url<-as.data.frame(news_url) #데이터프레임으로 만들기
df_news_url<-
  df_news_url[which(grepl("news.naver.com",df_news_url$news_url)),]  #인덱스를 활용한 행 추출
df_news_url<-as.data.frame(df_news_url)
df_news_url<-df_news_url[!grepl("sports",df_news_url$df_news_url),] #스포츠뉴스제거 
df_news_url<-as.data.frame(df_news_url) #열 이름 바꾸기
names(df_news_url)<-"url"
NEWS1<-df_news_url
##제목, 날짜, 본문 수집
 #데이터프레임 내의 빈 열 만들기
NEWS1$title <- ""
NEWS1$date <- ""
NEWS1$content <- ""
View(NEWS1)
NEWS1$url[1] #직접 접속
html <- read_html(as.character(NEWS1$url[1])) 
html_nodes(html, "#articleTitle") %>% html_text() #제목
html_nodes(html, ".t11") %>% html_text() #시간
html_nodes(html, "#articleBodyContents") %>% html_text() #내용
dim(NEWS1)#url구조
for (i in 1:124){
  html <- read_html(as.character(NEWS1$url[i]))
  temp_news_title<- html_nodes(html, "#articleTitle") %>% html_text()
  temp_date <- html_nodes(html, ".t11") %>% html_text()
  temp_news_content <- html_nodes(html, "#articleBodyContents") %>% html_text()
  if (length(temp_news_title)>0){
    NEWS1$title[i] <- temp_news_title
    NEWS1$date[i] <- temp_date
    NEWS1$content[i] <- temp_news_content 
    }
  print(i) ##진행상황 확인
  }
View(NEWS1)
##빈칸으로 수집된 것은 연예뉴스가 아님
NEWS1_1 <- NEWS1
NEWS1_1<- NEWS1_1[grepl(" ", NEWS1_1$title),]
setwd("/Users/changmi-ra/Documents/R_data")
write.csv(NEWS1_1, "네이버뉴스_장미라.csv", row.names=F)


#검색어, 날짜별 크롤링
library(httr)
library(rvest)
library(dplyr)
library(stringr)
url='https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EC%98%81%ED%99%94%20%EB%93%84&sort=0&photo=0&field=0&pd=3&ds=2021.10.20&de=2021.10.27&cluster_rank=252&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:from20211020to20211027,a:all&start='
naver_url_list<-c()
naver_url_list
for(i in 1:35)
  {i=(i*10)-9
  naver_url=paste(url,i,sep='')
  naver_url_list<-c(naver_url_list,naver_url)}
head(naver_url_list,5)
news_url<-c()
for(i in 1:length(naver_url_list)){
  naver_url<-naver_url_list[i]
  html<-read_html(naver_url)
  temp<-unique(html_nodes(html,'.info_group')%>%
                 html_nodes('a')%>%
                 html_attr('href'))
  news_url<-c(news_url,temp)
  print(i)
  }
View(news_url)
#네이버 뉴스만 추출, 스포츠뉴스 제거
df_news_url<-as.data.frame(news_url) #데이터프레임으로 만들기
df_news_url<-
  df_news_url[which(grepl("news.naver.com",df_news_url$news_url)),]  #인덱스를 활용한 행 추출
df_news_url<-as.data.frame(df_news_url)
df_news_url<-df_news_url[!grepl("sports",df_news_url$df_news_url),] #스포츠뉴스제거 
df_news_url<-as.data.frame(df_news_url) #열 이름 바꾸기
names(df_news_url)<-"url"
NEWS1<-df_news_url
##제목, 날짜, 본문 수집
#데이터프레임 내의 빈 열 만들기
NEWS1$title <- ""
NEWS1$date <- ""
NEWS1$content <- ""
View(NEWS1)
NEWS1$url[1] #직접 접속
html <- read_html(as.character(NEWS1$url[1])) 
html_nodes(html, ".end_tit") %>% html_text() #제목
html_nodes(html, ".author:nth-child(1) em") %>% html_text() #시간
html_nodes(html, "#articeBody") %>% html_text() #내용
dim(NEWS1)#url구조
for (i in 1:166){
  html <- read_html(as.character(NEWS1$url[i]))
  temp_news_title<- html_nodes(html, ".end_tit") %>% html_text()
  temp_date <- html_nodes(html, ".author:nth-child(1) em") %>% html_text()
  temp_news_content <- html_nodes(html, "#articeBody") %>% html_text()
  if (length(temp_news_title)>0){
    NEWS1$title[i] <- temp_news_title
    NEWS1$date[i] <- temp_date
    NEWS1$content[i] <- temp_news_content 
    }
  print(i) ##진행상황 확인
  }
View(NEWS1)
##빈칸으로 수집된 것은 연예뉴스가 아님
NEWS1_1 <- NEWS1
NEWS1_1<- NEWS1_1[grepl(" ", NEWS1_1$title),]
setwd("/Users/changmi-ra/Desktop/R-study")
write.csv(NEWS1_1, "네이버연애뉴스_영화듄.csv", row.names=F)
