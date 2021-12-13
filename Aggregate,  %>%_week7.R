##7-2 : Aggregate, %>% 함수
setwd("/Users/changmi-ra/Desktop/R-study")
library(googleVis)
# aggregate 함수
Fruits #데이터 꺼내기
aggregate(Sales~Year, Fruits, sum) #년도에 따른 판매가, 과일에 대한, 합계
aggregate(Sales~Year, Fruits, max)
aggregate(Sales~Fruit+Year, Fruits, max) #조건 2개 :과일의 이름별, 연도별 최대 수량 구하기.

# %>% 함수
library(dplyr) # %>% 함수 쓸 수 있음
exam<-read.csv("exam_week_7.csv") # 데이터 불러오기
exam%>%arrange(desc(math)) #exam을 출력, 정렬, 내림차순, 수학; 수학을 내림차순으로 정렬해라
exam %>%
  group_by(class) %>% #클래스별로 분리 후
  summarise(mean_math = mean(math)) #수학 평균 
exam %>%
  group_by(class) %>% #클래스별로 분리 후
  summarise(mean_math = mean(math), #수학 평균 
            sum_math = sum(math), #수학 합계
            median_math = median(math), #수학 중앙값
            n=n()) 

##7-3 : 실습
getwd()
setwd("/Users/changmi-ra/Desktop/R-study")
body<-read.csv("assignment_week_7.csv")
aggregate(height~Year+affiliation,body,max) #조건별로 묶는 코드

library(dplyr) # %>% 쓰려면 필요
body%>%group_by(Year,affiliation)%>%summarise(max_height=max(height)) #조건별로 묶는 코드-에러

#구글모션 차트-에러(지원안됨)
library(googleVis)
chart<-read.csv("assignment_week_7.csv")
chart
t2<-gvisMotionChart(chart,idvar = "weight",timevar = "Year", 
                    options = list(width=1000, height=500))
plot(t2)
