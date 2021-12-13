##14-2 : 군집분석
member<-data.frame(spent= c(10, 1, 1, 17, 4, 6, 1, 15, 22, 3, 0, 3, 7, 0, 2),
                   time= c(15, 2, 10, 7, 5, 7, 1, 10, 18, 3, 1, 3, 7, 10, 2)) #데이터
result<-kmeans(member,3) #3개의 군집
result

install.packages("fpc")
library(fpc)
plotcluster(member, result$cluster, color=TRUE, shade=TRUE) #표 만들기

result$withinss #응집도
result$cluster #어느 클러스터에 속해있는지
result$centers #각 클러스터의 센터가 어떻게 되는지
result$totss #총 sum of square
result$tot.withinss #withiness의 총합
result$betweenss #다른 클러스터간 얼마나 떨어져 있는지
result$size #각 클러스터에 속한 데이터 갯수
result$iter #얼마나 반복을 했는지

member$cluster<-result$cluster
aggregate(data= member, spent~cluster, mean)
aggregate(data= member, spent~cluster, max)

##14-3 : 군집분석 실습
#1단계: 데이터 읽기
iris #데이터
#1단계: 분포 파악 
plot(iris$Sepal.Length, iris$Sepal.Width,
     xlab = "꽃받침 길이", ylab = "꽃받침 너비",
     col= as.numeric(factor(iris$Species))) #꽃받침의 길이와 너비 간 산포도 출력. 종을 요인으로 인식함.
#2단계: 데이터 표준화
data.scaled<-as.data.frame(scale(iris[,-5], center = TRUE,
                                 scale = TRUE)) #1~4열에 해당하는 데이터의 편차 표준화(5열 제외)
head(data.scaled)
#3단계: 군집화
kc<-kmeans(data.scaled, 3) #군집화 
kc #군집화 결과
kc$cluster #각 종의 군집
kc$center #군집 중심
kc$withinss #WSS
kc$tot.withinss #TSS 
plot() #군집 그래프 결과 
#4단계: 군집화 결과 출력
par(mar = c(5.1, 4.1, 4.1, 7)) #그래프 바깥 영역의 마진 크기
plot(data.scaled[,1], data.scaled[,2],
     xlab = "꽃받침 길이", ylab = "꽃받침 너비",
     pch = 21, col = kc$cluster) #꽃받침의 길이와 너비의 산포도. 기호:모양(21), 색(군집번호)
legend("topright", legend = levels(iris$Species),
       pch = 21, col = kc$cluster,
       xpd = TRUE, inset = c(-0.5, 0)) #범례의 위치
points(kc$centers,
       pch = 19, cex = 1.5, col = rownames(kc$centers)) #군집 중심. 기호: 모양(19), 크기(1.5배)
#정확도
table(kc$cluster, iris[,5])
#0단계: 군집 수의 변화에 따른 TWSS의 변화
TWSS<-NULL
for(i in 1:15){
  kc<-kmeans(data.scaled, centers = i)
  twss<-c(twss, kc$tot.withinss)
}
plot(1:15, twss,
     xlim=c(0,15), type = "b",
     xlab = "군집수", ylab = "TWSS", family="AppleGothic")




