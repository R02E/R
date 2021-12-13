##10-1 : 데이터 시각화
setwd("/Users/changmi-ra/Desktop/R-study")
library(foreign)
library(dplyr)
library(ggplot2)
raw_welfare<-read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
welfare<-raw_welfare #원본 데이터는 두고, welfare 그릇에 담기
welfare<-rename(welfare,
                sex=h10_g3,
                birth=h10_g4,
                marriage=h10_g10,
                religion=h10_g11,
                income=p1002_8aq1,
                code_job=h10_eco9,
                code_region=h10_reg7)
class(welfare$sex)
table(welfare$sex)
welfare$sex<-ifelse(welfare$sex==1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)+xlim(0,1000)

sex_income<-welfare%>%
  filter(!is.na(income))%>%
  group_by(sex)%>%
  summarise(mean_income=mean(income))
sex_income
ggplot(data = sex_income, aes(x=sex, y=mean_income))+geom_col()

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

#유튜브 예시
youtube<-read.csv("youtube_week10.csv")
View(youtube)
youtube$ratio<-(youtube$dislikeCount/(youtube$dislikeCount+youtube$likeCount))*100
youtube$ratio_ch<-ifelse(youtube$ratio<25, 1, ifelse(youtube$ratio<50,2,
                                                     ifelse(youtube$ratio<75,3,4)))
table(youtube$ratio_ch)
View(youtube)
ratio_commentCount<-youtube%>%
  filter(!is.na(commentCount))%>%
  group_by(ratio_ch)%>%
  summarise(mean_commentCount=mean(commentCount))
ratio_commentCount
ggplot(ratio_commentCount, aes(x=ratio_ch, y=mean_commentCount))+geom_col()

youtube$ratio<-(youtube$dislikeCount/(youtube$dislikeCount+youtube$likeCount))*100 #비율 구하기
youtube$ratio_ch<-ifelse(youtube$ratio<25, 1, ifelse(youtube$ratio<50,2,
                                                     ifelse(youtube$ratio<75,3,4)))
table(youtube$ratio_ch) #비율 별 갯수 

ratio_commentCount<-youtube%>%
  filter(!is.na(commentCount))%>%
  group_by(ratio_ch)%>%
  summarise(mean_commentCount=mean(commentCount)) #댓글 갯수 비율 구하기
ratio_commentCount
ggplot(ratio_commentCount, aes(x=ratio_ch, y=mean_commentCount))+geom_col()


## 10-2 : 선형회귀, 회귀분석
setwd("/Users/changmi-ra/Desktop/R-study")
men<-read.csv("men_week10.csv")
men
plot(weight~height, data = men) #plot 그래프 그리기
m<-lm(weight~height, data = men)
abline(m, col="red") #회귀선 그리기
summary(m) #r-squared 값 구하기
cor.test(men$weight, men$height) #상관계수 값 구하기

