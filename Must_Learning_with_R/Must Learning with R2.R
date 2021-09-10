HR = read.csv("./HR_comma_sep.csv")
#head() : 데이터 윗부분을 출력하는 명령어
head(HR, n=3)
str(HR) # 각 변수들이 어떤 strings를 가지는지 확인을 하는 것 
#요약된 데이터 살펴보기 
summary(HR)

summary(HR$left)

HR$Work_accident = as.factor(HR$Work_accident)
HR$left = as.factor(HR$left)
HR$promotion_last_5years = as.factor(HR$promotion_last_5years)

summary(HR$left)

# ifelse(조건, TRUE, FALSE)
HR$satisfaction_level_group_1 = ifelse(HR$satisfaction_level > 0.5,
       'High', 'Low')
HR$satisfaction_level_group_1 = as.factor(HR$satisfaction_level_group_1)

summary(HR$satisfaction_level_group_1)

HR$satisfaction_level_group_2 = ifelse(HR$satisfaction_level > 0.8,
       'High', ifelse(HR$satisfaction_level > 0.5,
                      'Mid', 'Low'))
HR$satisfaction_level_group_2 = as.factor(HR$satisfaction_level_group_2)

summary(HR$satisfaction_level_group_2)

#subset() : 조건에 맞는 데이터를 추출하는 명령어
#subset(데이터, 추출 조건)
HR_High = subset(HR, salary == 'high')
summary(HR_High$salary)

HR_High_IT =  subset(HR, (salary == 'high')& (sales == 'IT'))
print(xtabs(~ HR_High_IT$sales + HR_High_IT$salary))

HR_High_IT2 = subset(HR, salary == 'high' | sales == 'IT')
print(xtabs(~ HR_High_IT2$sales + HR_High_IT2$salary))

# install.packages('plyr')
library(plyr)

#ddply(데이터, 집계기준, summarise, 요약변수)
SS=ddply(HR, c('sales', 'salary'), summarise,
         M_SF = mean(satisfaction_level),
         COUNT = length(sales),
         M_WH = round(mean(average_montly_hours), 2))
SS

# ggplot2 
# 1. 축을 그린다 2. 그래프를 그린다 3. 범례, 제목, 글씨 등 기타 옵션을 수정한다.
# aes = aesthetic(미적) ; 그래프에 변수를 설정 (축 설정)
# ggplot2에 들어갈 변수들은 모두 aes()안에 들어간다.

# geom_bar() : 막대도표 
# geom_histogram() : 히스토그램
# geom_boxplot() : 박스플롯 
# geom_line() : 선 그래프 
# labs() : 범례 제목 수정 
# ggtitle() : 제목 수정 
# xlabs(), ylabs(), x축, y축 이름 수정

library(ggplot2)
# library(ggthemes)

HR$salary = factor(HR$salary, levels = 
                     c('low', 'medium', 'high'))

# 막대 도표 : 이산형 변수 하나를 집계 내는 그래프, 1차원
ggplot(HR)
ggplot(HR,aes(x=salary))
ggplot(HR, aes(x=salary)) +
  geom_bar(aes(fill = salary))

ggplot(HR, aes(x=salary)) +
  geom_bar(aes(fill=left)) # left 값에 따라 색 채우기

# 점,선처럼 면적이 없는 그래프는 col옵션을 통해 색을 바꿔주며,
# 면적이 있는 그래프들은 fill옵션을 통해 색을 변경한다.

# 히스토그램 : 연속형 변수 하나를 집계 내는 그래프, 1차원 
ggplot(HR, aes(x=satisfaction_level)) +
  geom_histogram()

ggplot(HR, aes(x=satisfaction_level)) +
  geom_histogram(binwidth =  0.01, col='red',
                 fill='royalblue')

# 밀도그래프 : 연속형 변수 하나를 집계 내는 그래프, 1차원
ggplot(HR, aes(x=satisfaction_level)) +
  geom_density()

ggplot(HR, aes(x=satisfaction_level)) +
  geom_density(col='red', fill='royalblue')
# col=테두리, fill=채우기 박스플롯 

# 박스플롯 : 이산형 변수에 따라 연속형 변수의 분포 차이를
# 표현해주는 2차원 그래프
ggplot(HR, aes(x=left, y=satisfaction_level)) +
  geom_boxplot(aes(fill = left)) +
  xlab('이직여부') + ylab('만족도') + ggtitle('Boxplot') +
  labs(fill = '이직 여부')

ggplot(HR, aes(x=left, y=satisfaction_level)) +
  geom_boxplot(aes(fill = left), alpha = I(0.4)) +
  geom_jitter(aes(col = left), alpha = I(0.4)) +
  xlab('이직여부') + ylab('만족도') + ggtitle('Boxplot') +
  labs(fill = '이직 여부')

ggplot(HR, aes(x=left, y=satisfaction_level)) +
  geom_boxplot(aes(fill = salary), alpha = I(0.4), outlier.colour = 'red') +
  xlab('이직여부') + ylab('만족도') + ggtitle('Boxplot') +
  labs(fill = '임금 수준')

# 산점도 : 두 연속형 변수의 상관관계를 표현해주는 2차원 그래프 
# 산점도는 모델링 전에 변수들 간의 관계를 파악하는데 있어, 가장 효과적이다.
ggplot(HR, aes(x=average_montly_hours, y=satisfaction_level)) +
  geom_point(aes(col = left)) +
  labs(col = '이직 여부') + xlab('평균 근무 시간') + ylab('만족도')

# summary = 변수에 대한 요약 값
summary(HR$salary)
summary(HR$satisfaction_level)

# quantile : 분위수
quantile(HR$satisfaction_level, 
         probs = c(0.1, 0.3, 0.6, 0.9))

sum(HR$satisfaction_level)
mean(HR$last_evaluation)
sd(HR$satisfaction_level)

# 행별로 합, 평균 구할 시에는 rowSums, rowMeans 활용 
colMeans(HR[1:5])
colSums(HR[1:5])

# 빈도 테이블 작성
# table = 각 빈도수 보여줌
TABLE = as.data.frame(table(HR$sales))

TABLE2 = as.data.frame(xtabs(~ HR$salary + HR$sales))
