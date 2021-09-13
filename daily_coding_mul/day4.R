# 거리행렬

matrix()

# 100개의 정규분포 랜덤난수 생성
a=matrix(rnorm(100), nrow=5) # 5행 20열
#5건 데이터, 20차원으로 구성

# dist : 거리 행렬 구하기
plot(hclust(dist(a), method = 'single'))
# single : 최단 거리 
# complete : 최장거리 
# average : 평균
plot(hclust(dist(a), method = 'complete'))

plot(hclust(dist(a), method = 'average'))

# install.packages('mlbench')
library(mlbench)
data(Glass)
Glass

mydata = Glass
head(mydata, 5)
unique(mydata$Type)
table(mydata$Type)

# 수치데이터만 스케일 
# -를 붙이면 없어진다.
s_data= scale(mydata[, -10])
s_data
hc=hclust(dist(s_data), method = 'average')
plot(hc, hang=-1)

#7개의 클러스터로 나눌 경우
rect.hclust(hc, k=7, border='blue')

# 최적의 클러스터 개수를 구함
install.packages('fpc')
library(fpc)

# 20개의 0으로 초기화
opt_clust=numeric(20)
opt_clust



# k는 클러스터의 개수
# cluster:: 에 있는 pam을 가져온다.
for(k in 2:20){
  opt_clust[k]=cluster::pam(s_data, k)$silinfo$avg.width
}
max(opt_clust)
best_num=which.max(opt_clust)
best_num

#print와 같은 의미
cat('최적의 클러스터 개수: ', best_num)

# cluster::pam(s_data, 2)$silinfo$avg.width
# 실루엣 값(0~1) : 0.4112153
# 실루엣 값이 1에 가까울수록 클러스터링 잘됨

#kmearns 클러스터링 - 엘보우 그래프 
library(ggplot2)
iris
str(iris)
# 5번째 열 제외
irisScale=scale(iris[,-5])
irisScale

ggplot(iris, 
       aes(x=Sepal.Length, y=Sepal.Width, col=Species))+
  geom_point()

ggplot(iris, 
       aes(x=Petal.Length, y=Petal.Width, col=Species))+
  geom_point()

kmax=10
wss=rep(NA, kmax)
wss
rClust=list()


for(i in 1:kmax){
  irisCluster=kmeans(irisScale, i)
  wss[i]=irisCluster$tot.withinss
  rClust[[i]]=irisCluster$size
  #withinss : 클러스터 내의 데이터 사이 거리 제곱 합
  #tot.withinss : withinss 의 총합
}
rClust

plot(1:kmax, wss, type='b')

res=kmeans(irisScale, 3)
str(res)

plot(iris, col=res$cluster)

iris$Species
res$cluster

table(Predicted=res$cluster, Actual=iris$Species)

# knn : k개 이웃을 참조 - 관심 대상 데이터의 예측

# 국 영 수 합/불
# 50 50 50 불 
# 90 90 90 합 
# ...
# 
# 100 100 100 ???
#   ex) k가 3이라면, 가장 가까운 3명의 데이터 검색(유클리디안 거리)
# -> 합/불 다수결? 합격 : 2, 불합격 : 1=> 합격 분류
# 
# 국 영 수 과
# 50 50 50 50
# 90 90 90 90 
# ...
# 100 100 100 ???
#   ex) k가 3이라면, 가장 가까운 3명의 데이터 검색(유클리디안 거리)
# -> 과학점수? 3명 과학점수 -> 90, 95, 90 -> 92... => 예측

# https://archive.ics.uci.edu/ml/datasets.php

wbcd=read.csv('f:/data/wisc_bc_data.csv')
str(wbcd)

#첫번째 열을 제외한 나머지 열
wbcd=wbcd[-1] #id열 제외
str(wbcd)
table(wbcd$diagnosis)

factor(wbcd$diagnosis) # factor : 범주형으로 바꿈

# wbcd$diagnosis # 문자형

factor(wbcd$diagnosis, labels = c('Benign', 'Malignant'))

# 순서에 맞게 기술 
wbcd$diagnosis= factor(wbcd$diagnosis, labels = c('Benign', 'Malignant'), levels=c('B', 'M'))

# 비율
prop.table(table(wbcd$diagnosis))

round(prop.table(table(wbcd$diagnosis))*100,1)

# 기술통계치
summary(wbcd)

summary(wbcd['radius_mean'])
# 보고 싶은 열을 벡터로 만든다.
summary(wbcd[c('radius_mean', 'area_mean', 'smoothness_mean')])

normalize<-function(x){
  return ((x-min(x)) / (max(x) - min(x)))
}
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

head(wbcd, 3)

# lapply는 리스트형식으로 반환 
# l = list
wbcd_n=as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n)

class(lapply(wbcd[2:31], normalize))

# 벡터, 행렬, 리스트, 배열

str(wbcd_n)
nrow(wbcd_n)

# 1번~469번까지 데이터 -> knn 모델 생성 
# 470~569번까지 데이터를 -> knn 모델에 입력 -> 출력(예상되는 진단 결과)



# apply 함수 : 벡터, 행렬,리스트, 데이터프레임 -> 함수 적용 -> 결과 도출
# 1) apply함수 : 행렬의 행 또는 열 방향 함수 적용
sum(1:10)

m=matrix(1:9, ncol=3)
# 두번째는 방향을 의미함 (=margin)
#1: 행을 기준으로 
#2: 열을 기준으로
apply(m, 1, sum)
apply(m, 2, sum)
head(iris)
apply(iris[,-5], 2, sum)
colSums(iris[,1:4])

# lapply(벡터/리스트, 함수)
myFunc = function(x){
  x*2
}
myFunc(3)

lapply(1:3, myFunc)
unlist(res)

lapply(1:3, function(x){x*2})

lapply(iris[,1:4], sum)
lapply(iris[,1:4], mean)
as.data.frame(matrix(unlist(lapply(iris[,1:4], mean),2)))

# sapply(): lapply와 거의 똑같음. 차이점은 결과가 벡터 또는 행렬로 나옴

lapply(iris[,1:4], mean)
sapply(iris[,1:4], mean)

# tapply() : 그룹별로 연산하는 apply함수
# tapply()

a=1:10
1:10 %% 2
a[a %% 2 == 1]

table(1:10 %% 2 == 1)

# ex) 1~10까지 수에 대해 짝수그룹/홀수그룹 각각의 합
a=1:10
sum(a[a %% 2 == 0])
sum(a[a %% 2 == 1])

tapply(1:10, a%%2==1, mean)
a%%2==1

#iris에서 Species 별로 Sepal.Length의 평균
tapply(iris$Sepal.Length, iris$Species, mean)


wbcd_train_labels=wbcd[1:469,1] #knn 모델 생성 과정 사용 데이터 
wbcd_test_labels=wbcd[470:569,1] # 모델 테스트 데이터 생성 
# knn()

wbcd_train=wbcd_n[1:469,]
wbcd_test=wbcd_n[470:569,]
library(class)
pred=knn(train=wbcd_train, test=wbcd_test,
    cl=wbcd_train_labels, k=21)
pred #테스트 데이터에 대한 예측값값
wbcd_test_labels #실제값값

install.packages('gmodels')
library(gmodels)

CrossTable(x=wbcd_test_labels, y=pred) 
#교차표(예측값과 실제값을 비교)


write.csv(data.frame(pred), 'pred.csv')

head(read.csv('F:/data/titanic/train.csv'),2)
head(read.csv('F:/data/titanic/test.csv'),2)
head(read.csv('F:/data/titanic/gender_submission.csv'),2)

# df=data.frame(PassengerId=아이디저장된변수 이름, Survived=예측결과가 저장된 변수이름 )
# write.csv(df, '제출파일명.csv')

# df=data.frame(PassengerId=, Survived=예측결과가 저장된 변수이름 )

