library(dplyr)                
library(ggplot2)


bike=read.csv('f:/data/SeoulBikeData.csv')
str(bike)
View(bike)


# y = Rented.Bike.Count
# 분석할 가치가 있는 column = Date, Hour, Temperature.캜. , Humidity... , Wind.speed..m.s. , Seasons , Holiday

# 1. 전처리(결측 처리)
sum(is.na(bike))
bike=na.omit(bike)
colSums(is.na(bike))

# Date 
bike$Date=as.Date(bike$Date)
bike$Month = as.factor(format(bike$Date, '%m'))
bike$Day = as.factor(format(bike$Date, '%a'))

str(bike)

# Hour
summary(bike$Hour)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    5.75   11.50   11.50   17.25   23.00

# Temperature.캜. Humidity...  Wind.speed..m.s. 변수명 수정 
bike <- rename(bike, Temperature = Temperature.캜.)
bike <- rename(bike, Humidity = Humidity...)
bike <- rename(bike, WindSpeed = Wind.speed..m.s.)

# Seasons
bike$Seasons=ifelse(bike$Seasons == 'Spring', 1,
                    ifelse(bike$Seasons == 'Summer', 2,
                           ifelse(bike$Seasons == 'Autumn', 3, 4)))

bike$Seasons=as.factor(bike$Seasons)

# Holiday
bike$Holiday = ifelse(bike$Holiday == 'Holiday', 1, 2)

bike$Holiday = as.factor(bike$Holiday)

# 2. 상관계수

# 분석할 column만 뽑기
bike2=bike %>% 
  select(Rented.Bike.Count, Hour, Temperature, Humidity, WindSpeed, Seasons, Holiday)

bike2 = bike2 %>% 
  mutate_if(is.integer, as.numeric)

str(bike2)
# 'data.frame':	8760 obs. of  7 variables:
#   $ Rented.Bike.Count: int  254 204 173 107 78 100 181 460 930 490 ...
# $ Hour             : int  0 1 2 3 4 5 6 7 8 9 ...
# $ Temperature      : num  -5.2 -5.5 -6 -6.2 -6 -6.4 -6.6 -7.4 -7.6 -6.5 ...
# $ Humidity         : int  37 38 39 40 36 37 35 38 37 27 ...
# $ WindSpeed        : num  2.2 0.8 1 0.9 2.3 1.5 1.3 0.9 1.1 0.5 ...
# $ Seasons          : Factor w/ 4 levels "1","2","3","4": 4 4 4 4 4 4 4 4 4 4 ...
# $ Holiday          : Factor w/ 2 levels "1","2": 2 2 2 2 2 2 2 2 2 2 ...

cor(bike2[,c(1,2,3,4,5)], use = "all.obs", method = "pearson")
#                       Rented.Bike.Count      
# Rented.Bike.Count         1.0000000
# Hour                      0.4102573
# Temperature               0.5385582 
# Humidity                 -0.1997802 
# WindSpeed                 0.1211084


# -자전거 대여 수와 다른 변수들간의 관계
# 3. 시각화

# Hour
ggplot(bike2, aes(x=Hour)) +
  geom_density()

ggplot(bike2, aes(x=Hour, y=Rented.Bike.Count)) +
  geom_point(aes(fill=Hour), colour='blue') + 
  theme_classic()
# 대체로 15~18시간 정도 자전거를 빌려서 탄다.
# 0~5시간 정도 빌리는 경우는 좀 낮다. 


# Temperature
ggplot(bike2, aes(x=Temperature)) +
  geom_density()

ggplot(bike2, aes(x=Temperature, y=Rented.Bike.Count)) +
  geom_point(aes(fill=Temperature), colour='blue') + 
  theme_classic()
# 20도 부근일 때 사람들이 많이 자전거를 빌린다. 
# 20도를 중심으로 정규분포 형태를 띈다.


# Humidity
ggplot(bike2, aes(x=Humidity)) +
  geom_density()

ggplot(bike2, aes(x=Humidity, y=Rented.Bike.Count)) +
  geom_point(aes(fill=Humidity), colour='blue') + 
  theme_classic()
# 습도는 50 부근일 때 가장 많이 빌린다. 적당할 때 많이 빌리는 걸 알 수 있다.


# WindSpeed
ggplot(bike2, aes(x=WindSpeed)) +
  geom_density()

ggplot(bike2, aes(x=WindSpeed, y=Rented.Bike.Count)) +
  geom_point(aes(fill=WindSpeed), colour='blue') + 
  theme_classic()
# 바람 부는 속도는 2~3일 때 자전거를 가장 많이 빌린다.


# Seasons
ggplot(bike2, aes(x=Seasons)) +
  geom_bar(aes(fill=Seasons)) + 
  theme_classic() # 골고루 퍼져 있다.

ggplot(bike2, aes(x=Seasons, y = Rented.Bike.Count)) +
  geom_boxplot(aes(fill=Seasons)) + 
  theme_classic()
# 바이크 빌리는 경우 : 여름 > 가을 > 봄 > 겨울

# Holiday
ggplot(bike2, aes(x=Holiday)) +
  geom_bar(aes(fill=Holiday)) + 
  theme_classic()

ggplot(bike2, aes(x=Holiday, y = Rented.Bike.Count)) +
  geom_boxplot(aes(fill=Holiday)) + 
  theme_classic()
# 1은 Holiday(휴일), 2은 Non holiday(평일)
# 휴일보다 평일에 바이크를 더 빌리는 것을 알 수 있다.


# 4. 회귀모델
# 1)회귀모델 <- RBC ~ 독립변수 조합 
model1=lm(Rented.Bike.Count ~ Hour + Temperature + Humidity + WindSpeed + Seasons + Holiday, bike2)
# lm(formula = Rented.Bike.Count ~ Hour + Temperature + Humidity + 
#      WindSpeed + Seasons + Holiday, data = bike2)
# 
# Coefficients:
#   (Intercept)         Hour  Temperature     Humidity    WindSpeed     Seasons2     Seasons3     Seasons4     Holiday2  
#       462.657       28.567       20.500       -7.600       -3.136       71.484       72.651     -246.232      128.136  

summary(model1)
# lm(formula = Rented.Bike.Count ~ Hour + Temperature + Humidity + 
#      WindSpeed + Seasons + Holiday, data = bike2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1395.48  -276.23   -56.95   223.86  2327.74 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  462.6569    35.7577  12.939  < 2e-16 ***
#   Hour          28.5669     0.7864  36.325  < 2e-16 ***
#   Temperature   20.4998     0.8660  23.671  < 2e-16 ***
#   Humidity      -7.5996     0.2753 -27.609  < 2e-16 ***
#   WindSpeed     -3.1356     5.3388  -0.587 0.557006    
#   Seasons2      71.4840    18.5922   3.845 0.000121 ***
#   Seasons3      72.6513    14.3622   5.059 4.31e-07 ***
#   Seasons4    -246.2323    19.9998 -12.312  < 2e-16 ***
#   Holiday2     128.1361    23.4256   5.470 4.63e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 469.8 on 8751 degrees of freedom
# Multiple R-squared:  0.4699,	Adjusted R-squared:  0.4694 
# F-statistic: 969.6 on 8 and 8751 DF,  p-value: < 2.2e-16

# 해석 : F-statistic: 969.6 on 8 and 8751 DF,  p-value: < 2.2e-16 이므로 
# 이 모형은 유의하다.

# 다중공선성 파악 
library(car)
vif(model1)
# GVIF Df GVIF^(1/(2*Df))
# Hour        1.176017  1        1.084443
# Temperature 4.246153  1        2.060620
# Humidity    1.246526  1        1.116479
# WindSpeed   1.214585  1        1.102082
# Seasons     4.485558  3        1.284210
# Holiday     1.020970  1        1.010431

# Temperature, Seasons은 vif가 4가 넘지만 제거할 수준은 아니라 판단



# 2)회귀모델로 예측 -> 예측 결과와 실제값 사이의 차이가 어느정도인지 조사

# WindSpeed는 무의미하므로 제거 후 다시 실행
model2=lm(Rented.Bike.Count ~ Hour + Temperature + Humidity + Seasons + Holiday, bike2)
summary(model2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  455.5232    33.6303  13.545  < 2e-16 ***
#   Hour          28.4673     0.7679  37.072  < 2e-16 ***
#   Temperature   20.4823     0.8655  23.666  < 2e-16 ***
#   Humidity      -7.5566     0.2653 -28.478  < 2e-16 ***
#   Seasons2      72.2846    18.5414   3.899 9.75e-05 ***
#   Seasons3      73.8442    14.2173   5.194 2.10e-07 ***
#   Seasons4    -246.2619    19.9990 -12.314  < 2e-16 ***
#   Holiday2     128.2432    23.4240   5.475 4.50e-08 ***

# seasons2=summer, seasons3=autumn, holiday2=평일

train = bike2[c(1:50),]
predict(model2, newdata=train)



# EX)
# 17년 12월 1일 ~ 18년 10월 31일 -> 모델
# 
# 18년 11월 1일 ~ 18년 11월 30일 -> 테스트 데이터 =>sum(자전거 대여수 예상 - 실제 대여수)