## 9-1 : 데이터 시각화 treemap
setwd("/Users/changmi-ra/Desktop/R-study")
library("readxl") #엑셀 파일 읽으려면 필수
ck<-read_excel("restuarant_week9.xlsx")
head(ck)
addr<-substr(ck$소재지전체주소, 11, 16) #중복되는 글자 삭제, 12번째부터 15번째 글자만 사용한다
head(addr)
addr_num<-gsub("[0-9]", "", addr) #뒤의 번지수 지우기
addr_trim<-gsub(" ", "", addr_num) #뒤의 공백 지우기
head(addr_trim)
library(dplyr) # %>% 함수 사용
addr_count<-addr_trim%>%table()%>%data.frame() #table로 변수 갯수(도수분포표) 확인, data.frame으로 표 형태 구성
head(addr_count)

# 트리맵 만들기
library(treemap)
treemap(addr_count, index = ".", vSize = "Freq", title = "서대문구 동별 치킨집 분포", fontfamily.labels = "AppleGothic")


## 9-3 : 데이터의 집단간 비교 t-test




