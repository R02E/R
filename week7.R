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
