x = c(8, 3, 6, 6, 9, 4, 3, 9, 3, 4)
y = c(6, 2, 4, 6, 10, 5, 1, 8, 4, 5)
plot(x,y)

cov(x,y) # 공분산, 선형관계 측정

cor(x,y) # 피어슨 상관계수 
cov(x,y)/(sd(x)*sd(y)) #상관계수

cor(x,y) #피어슨 상관계수를 제곱하면 분산에서 공유하는 비율 

cor(x,y)**2 #상관계수 제곱값인 0.74의 의미?
#x의 분산중에서 74%를 y와 공유한다는 의미임 
#x의 분산중에서 74%가 y로 설명될 수 있음


cor(x,y, method='spearman')

cor(x,y, method='kendall') # 데이터 수가 충분치 않을 때

# p-value는 상관계수가 0이라고 했을 때, 같은 크기의 표본에서 
# 관찰된 상관계수보다 더 극단적인 상관계수가 관찰될 확률
# 0.05가 일반적인 임계치
cor.test(x,y) # p-value값 구하기


#w=z의 제곱
z = c(-3, -2, -1, 0, 1, 2, 3)
w = c(9, 4, 1, 0, 1, 4, 9)
cov(z,w) # 비선형관계는 표현하지 못함


cars
model=lm(dist ~ speed, data=cars)
# y=f(x), lm(종속 ~ 독립)
summary(model)

# Residuals = 예측값 - 실제값

# Estimate (추정값)

#-17.5791 : 절편 추정값 

# 3.9324 : speed의 계수 추정값, speed가 1증가 -> dist는 3.9324 증가
# dist=3.9324*speed-17.5791

# Multiple R-squared : 0.65
# 모델 적합도, dist를 speed가 약 65% 설명 

# Adjusted R-squared : 0.64
# 다중선형회귀분석 모델 적합도, 독립변수 여러개를 고려한 R-Squared

# 다중공선성(colinearity)
# 공선성 : 한 독립변수가 다른 하나의 독립변수를 잘 예측하는 경우(높은 상관성)
# 다중공선성 : 한 독립변수가 다른 여러개의 독립변수를 잘 예측하는 경우(높은 상관성)
# -> 회귀 계수가 무의미하게 나올 수 있음
# -> 회귀 계수가 불안정 => 예측 결과가 많이 빗나가게 됨 => 안 좋은 모델 생성

# 회귀분석 : 독립변수 -> 종속변수
# 시험점수(종속변수) = w1*공부시간 + w2*공부환경 + w3*집중도 + bias

# 다중회귀분석은 다중공선성이 제거되어야 함 
# -> 독립변수간 상관관계가 낮게(0.8미만) 나오도록 해야 함함

# 독립변수(x1,x2,x3,x4,x5), 종속변수(y)
# # 어떤 하나의 독립변수가 또 다른 하나의 독립변수와 완벽한 선형 독립이 아닌 경우에
# # 회귀모델에 다중공선성 문제가 존재한다 라고 함
# 
# y=w1x1+...+w5x5+b
# 
# 
# y=w1x1+w2x2+b
# x1 : 공부시간
# x2 : 진도량
# y : 성적
#  => x1과 x2는 양의 상관관계가 있음
# 
# x1 : 휴대폰사용시간
# x2 : 진도량
# y : 성적
# => x1과 x2는 음의 상관관계가 있음
# 
# x1 값의 변화가 y에만 영향을 미치는게 아니라, x2 값의 변화에도 영향을 줌
# y=w1x1+w2x2+b

#VIF(Variance Inflation) : 분산팽창요인 분석, 다중공선성 여부를 확인
# 일반적으로, IVF>10 => 다중공선성 있음 => 변수제거(정보 손실이 적은 변수 제거)
# 경우에 따라, IVF > 3 또는 4처럼 엄격하게 설정하기도 함

# VIF = 1 / (1 - R^2)
# R^2(R squared) : 0~1사이의 값, R^2이 커질수록 VIF는 커짐

# 예를 들어 w1x1+...+w5x5+b 다중 회귀식 경우에,
# R^2(R squared)의미는 
# 1) 독립변수 x1을 종속변수로 간주하고, 나머지 독립변수들을(x2~x5) 독립변수로 간주하고,
# 회귀분석을 실시. 이때 나오는 r squared 값
# => x1 변수가 다른 x2변수에 의해서 설명되는 부분이 많다면 r squared 값을 크게 나옴
# 
# x1=x2~x5
# x2=x1~x5
# ..
# x5=x1~x4
# 
# 
# 월급 = w1*경력 + w2*근속년수 + w3*교육훈련시수 + bias
# 
# 
# 근속년수 = w1*경력 +  w3*교육훈련시수+ bias
# ex) r^2 = 0.25
# 1 / (1-0.25) = 1/0.75 => > 1

df=read.csv('f:/data/crab.csv')

# fomula = target variable
model=lm(y ~ sat + weight + width, df)
summary(model) # 유의수준 5%(0.05)
#width, sat는 0.05보다 작으므로 유의미함

# install.packages('car')
library(car)
vif(model)

# 회귀계수가 통계적으로 유의미하지 않다면? Pr(>|t|) 값이 0.05보다 큰 경우
# (모델 변수가 유의미하다면, VIF가 크게 나오더라도 모델 사용 가능)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.936650   0.500403  -1.872   0.0630 .  
# sat          0.097113   0.008814  11.018   <2e-16 ***
#   weight      -0.046514   0.097890  -0.475   0.6353    
# width        0.053544   0.026465   2.023   0.0446 *  

# 회귀계수가 통계적으로 유의미하지 않다면,
# 변수를 배거나 새로운 변수를 생성
# ex) 국어점수 영어점수가 상관관계가 높다면 => 두 점수를 제거학, 새로운 변수를 
# 생성(언어점수)하거나, 두 점수 중에 하나를 뺀다(비추천)


# distress_ct : 파손된 링의 개수(타깃, y)
# 온도
# 압력
# 비행기 번호
# 
# 회귀모델 : 온도와 압력이 주어졌을 때, 몇 개의 링이 파손될까?
# 
# expenses : 의료비 지출금액(타깃, y)

chal=read.csv('f:/data/challenger.csv')
str(chal)
summary(chal)
table(chal$distress_ct)

plot(x=chal$temperature, y=chal$distress_ct)

lm_chal=lm(distress_ct ~ temperature, data=chal)
summary(lm_chal)

lm_chal$coefficients[1]
lm_chal$coefficients[2]

abline(a=lm_chal$coefficients[1], 
       b=lm_chal$coefficients[2], 
       col='blue')


insu = read.csv('f:/data/insurance.csv', stringsAsFactors = TRUE)
str(insu)
summary(insu)

boxplot(insu$expenses)
hist(insu$expenses)

#상관계수
#cor(x,y)
cor(insu$bmi, insu$expenses)
cor(insu[c('age','bmi','children','expenses')])
pairs(insu[c('age','bmi','children','expenses')])

# install.packages('psych')
library(psych)

pairs.panels(insu[c('age','bmi','children','expenses')])


# 다중선형회귀 
str(insu)
insu_model=lm(expenses ~ . , data=insu) # .은 expenses을 제외한 나머지 모든 변수
insu_model # 알아서 더미화 해줌

# sexmale : male이 1, female은 0
# smokeryes : yes가 1, no는 0

#지역은 원래 4개
#지역2 = 지역2는 1, 나머지는 0
#지역3 = 지역3는 1, 나머지는 0
#지역4 = 지역4는 1, 나머지는 0

# insu에 age열 값에 대한 제곱한 값을 갖는 age2라는 열을 추가
insu$age2=(insu$age)^2
insu$age2

bmi30=ifelse(insu$bmi >=30, 1, 0)

# 특성공학(feature engineering)

model=lm(expenses~age+sex+bmi30+age2, data=insu)
summary(model)

