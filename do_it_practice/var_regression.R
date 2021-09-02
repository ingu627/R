# 변수:
# -연속변수
# -범주변수(이산)
# 
# 척도(scale):
#   관측값을 나타내는 level(수준)
# 
# - 서열척도 : 관측값 사이에서 순서가 있음, 사칙연산이 불가능, 간격이 다름 
# -> 직급(사원, 대리, 과장, 부장,...)
# -> 사원과 대리 vs 과장과 부장 간의 차이(간격)가 다름
# 
# - 명명척도:관측값 사이에서 순서가 없음, 종류에 따른 빈도수 조사
# -> 혈액형, 성별
# 
# - 등간척도: 관측값 사이에서 순서가 있음, 간격이 동일, 연산 가능 
# -> 섭씨온도(20도는 10도의 2배, 2배 따뜻해지는 것은 아님)
# -> 섭씨(0도) <-> 화씨(32도)
# 
# - 비율척도
# -> 미터(20미터는 10미터의 2배, 2배 깊다)
# 
# ex) 나이 데이터 통계 분석
# 서열척도 분석 : 어린이 < 청소년 < 청년 < 장년 < 노인...

mydata<-read.csv('./data/examscore.csv', header=TRUE)
mydata$gender
mydata[,3]

mytable<-table(mydata$gender)
mytable
names(mytable)

pie(mytable, 
    labels=names(mytable),
    main='pie chart')
pie(mytable, 
    labels=c('여자', '남자'),
    main='파이 차트')
text(0.3, 0.3, '33.33%')
text(-0.3, -0.3, '66.67')

plot(mydata$midterm, mydata$final,
     xlab='중간점수',
     ylab='기말점수',
     main='제목')

mean(mydata$midterm)
median(mydata$midterm)

hist(mydata$midterm)
abline(v=mean(mydata$midterm), col='red') #직선을 그리는 함수
abline(v=median(mydata$midterm), col='blue')

quantile(mydata$midterm)

var(mydata$midterm) # 분산
sd(mydata$midterm) # 표준편차

# res<-table(mydata$final)
# print(names(sort(-res)[1]))


# 최빈값은 함수가 따로 없다.
# 함수 만들기
myMode <- function(x){
  imsi<-table(x)
  names(sort(-imsi)[1])
}
myMode(mydata$final)

rep(0,10) # 0을 10번 반복

a<-c(1:10)
a
a[1:5]<-c(5,3,1,4,2)
a <- ifelse(a>=5, 'high', 'low')
a

mydata$midterm
mydata$final

cor(mydata$midterm, mydata$final)

plot(mydata$midterm, mydata$final)
abline(v=mean(mydata$midterm))
abline(h=mean(mydata$final))

zx <-(mydata$midterm-mean(mydata$midterm))/sd(mydata$midterm)
zy <-(mydata$final-mean(mydata$final))/sd(mydata$final)

sum(zx*zy)/(length(mydata$midterm)-1) # 0.677

sign(zx*zy) # 부호만 나오게 해줌(양수(1), 음수(-1))

as.factor(sign(zx*zy)) #범주형식으로 만들어줌

c('blue', 'red')[as.factor(sign(zx*zy))]

plot(mydata$midterm, mydata$final,
     col=c('blue', 'red')[as.factor(sign(zx*zy))])
abline(v=mean(mydata$midterm))
abline(h=mean(mydata$final))

# 회귀분석: 독립변수(x)로 종속변수(y)를 예측
# 혼힙변수: 모델에는 포함되지 않지만, 종속변수에 영향을 미치는 변수
# 
# 모델 => 몸무게 = x*기우기 + 바이어스(절편)
# ex) 혼입변수 : 건강상태에 따라 식사량이 달라지고,
# 그에 따라 몸무게에 변화
# 운동량에 따라 식사량이 달라지고, 몸무게에 변화
# 
# 종속변수가 연속, 범주형(분류)
# 선형회귀분석 : 독립변수와 종속변수간에 직선관계
# 선형이란? 독립변수가 증가하면, 종속변수가 증가/감소
# 
# 종속변수(y)=기울기(w)*독립변수(x)+절편(b)
# 몸무게 = 200 *3그릇 + 0.1
# y=wx+b
# 
# w:회귀계수, 독립변수 x가 1씩 증가한다면 y는 w만큼 증가한다.
# b : 독립변수가 0이면 y=b, 절편, 독립변수가 0일때 종속변수 y값
# 
# 회귀 분석을 하는 이유??
# 회귀모델 -> 예측
# 
# 모델 예시 -> 점수= w*공부시간 + 기본점수
# 모델 적합도 : 공부시간과 점수의 관계가 데이터에 얼마나 잘 들어맞는지 검증

cars

plot(cars$speed, cars$dist)
scatter.smooth(cars) # 추세선이 있음음

# outliers가 있으면 회귀분석 결과가 왜곡됨

par(mfrow=c(1,2)) #1행 2열
boxplot(cars$speed, main='speed')
boxplot(cars$dist, main='dist')

#선형회귀분석은 독립변수와 종속변수가
# 정규분포를 따를 때 잘됨

par(mfrow=c(1,2)) #1행 2열
plot(density(cars$speed),main='speed')
plot(density(cars$speed),main='dist')

# install.packages('e1071')

library(e1071)
skewness(cars$speed) #왜도:데이터가 치우친 정도
skewness(cars$dist) #왜도:데이터가 치우친 정도
kurtosis(cars$speed)
kurtosis(cars$dist)

#왜도가 0, 첨도가 1일때 완전한 정규분포

# lm함수 : 회귀분석

# 종속변수 ~ 독립변수
# y=f(x)

lm(dist~speed, data=cars)
# y(dist)=3.932 * x(speed) + (-17.579)

# 정규분포? 평균을 중심으로 좌우가 대칭
# 왜도와 첨도로 정규성 확인
# 왜도? 비대칭성을 나타내는 척도


model<-lm(dist~speed, data=cars)
summary(model)

# Residuals(잔차): 실제값 - 예측값 
# Coefficients(회귀계수)
# Estimate(추정치)
# 
# Multiple R-squared : 0.6511의 의미?
# dist의 분산을 speed가 약 65%를 설명한다.
# 
# Adjusted R-squared: 다중회귀분석에 사용

#0+는 절편을 제거
model<-lm(dist~0+speed, data=cars)
summary(model)

par(mfrow=c(1,1))
mydata
plot(mydata$midterm, mydata$final)
abline(5, 2)
abline(-10, 2, col='red')
#기울기 1, 교차점 5
# y=2*x+5

# MSE : mean square error (모델y값 - 실제y값)

#잔차(residual):예측값-실제값
#RSS(residual sum of square) : 잔차 제곱의 합

mydata[, 3:4] # 행은 다 불러옴. 3열부터 4열까지

rss<-function(data, line_info){
  x<-data[,1]
  y<-data[,2]
  intercept<- line_info[1] #가상회귀모델 절편
  slope <- line_info[2] # 가상회귀모델 기울기
  HX <- slope*x+intercept
  result<-sum((y-HX)^2)
  result
  # print(HX) #예측 기말 점수
  # print(y) #실제 기말 점수
  # print(y-HX) #ERROR
  # sum((y-HX)^2) #Error의 제곱의 합 = cost
  #회귀모델의 목표는 cost를 최소로 하는 slope와 intercept을 찾는 것
  # 실제로는 함수가 다 만들어져 있다.
}


#가상의 회귀모델 =  y절편:5, 기울기:1
#가상의 회귀모델에 대한 RSS를 출력해보자.
rss(mydata[, 3:4], c(5,1))

res<-optim(par=c(1,1), fn=rss, data=mydata[, 3:4])
res

# par=c(1,1)은 초기값 설정
# fn은 최적화 함수를 알려줌

lm(final~midterm ,mydata )

# 기울기 = 상관계수 * (y의표준편차/x의 표준편차)
x<-mydata$midterm
y<-mydata$final
s<-cor(x,y)*(sd(y)/sd(x)) # 0.8966817
s
b<-mean(y)-s*mean(x) # 13.8866
b

#예측 테스트
#어떤 사람이 중간고사를 85점 받았다면 기말고사 예측?
yhat<-s*85+b
print(yhat)
