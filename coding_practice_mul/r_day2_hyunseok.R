# 1. 시험 점수 변수 만들고 출력하기
# 다섯 명의 학생이 시험을 봤습니다. 학생 다섯 명의 시험 점수를 담고 있는 변수를 만들어 출력해 보세요. 각
# 학생의 시험 점수는 다음과 같습니다.
# 80, 60, 70, 50, 90
score <- c(80, 60, 70, 50, 90)
score

# 2. 전체 평균 구하기
# 앞 문제에서 만든 변수를 이용해서 이 학생들의 전체 평균 점수를 구해보세요.
mean(score)


# 3. 전체 평균 변수 만들고 출력하기
# 전체 평균 점수를 담고 있는 새 변수를 만들어 출력해 보세요. 앞 문제를 풀 때 사용한 코드를 응용하면
# 됩니다.
score_mean <- mean(score)
score_mean


# 4. data.frame()과 c()를 조합해서 표의 내용을 데이터 프레임으로 만들어 출력해보세요.
# 제품 가격 판매량
# 사과 1800 24
# 딸기 1500 38
# 수박 3000 13
fruit <- data.frame(
  제품= c('사과','딸기','수박'), 
  가격=c(1800, 1500, 3000), 
  판매량=c(24,38,13))
fruit

# 5. 앞에서 만든 데이터 프레임을 이용해서 과일 가격 평균, 판매량 평균을 구해보세요
fruit_mean <- mean(fruit$가격)
fruit_mean

fruit_total_mean <- sum(fruit$가격 * fruit$판매량) / sum(fruit$판매량)
fruit_total_mean

# 6. mpg 데이터의 변수명은 긴 단어를 짧게 줄인 축약어로 되어있습니다. 
# cty 변수는 도시 연비, hwy 변수는고속도로 연비를 의미합니다. 
# 변수명을 이해하기 쉬운 단어로 바꾸려고 합니다. 
# mpg 데이터를 이용해서 아래 문제를 해결해 보세요.
# • Q1. ggplot2 패키지의 mpg 데이터를 사용할 수 있도록 불러온 뒤 복사본을 만드세요.
library(ggplot2)
library(dplyr)
m <- as.data.frame(ggplot2::mpg)
m

# • Q2. 복사본 데이터를 이용해서 cty는 city로, hwy는 highway로 변수명을 수정하세요.
m <- rename(m, city=cty, highway=hwy)
str(m)

# • Q3 자동차 배기량에 따라 고속도로 연비가 다른지 알아보자
m  %>% 
  group_by(city)  %>% 
  summarise(hightway_mean = highway)

# - displ(배기량)이 4 이하인 자동차와 5 이상인 자동차 중 어떤 자동차의 hwy(연비)가 평균적으로 높은가?
displ_4low <- m %>% filter((displ <= 4))
mean(displ_4low$highway)

displ_5more <- m %>% filter((displ >= 5))
mean(displ_5more$highway)

# • Q5. 자동차 제조 회사에 따라 도시 연비가 다른지 알아보자. 
# -"audi"와 "toyota" 중 어느 manufacturer의 cty가 평균적으로 높은지 알아 보자.
audi <- m %>% filter(manufacturer == 'audi')
mean(audi$city)

toyota <- m %>% filter(manufacturer == 'toyota')
mean(toyota$city)

# • Q6. 자동차 종류에 따라 도시 연비가 다른지 알아보자. 
# - class가 "suv"인 자동차와 "compact"인 자동차 중 어떤 자동차의 cty가 더 높은지 알아보자.

suv <- m %>% filter(class == 'suv')
mean(suv$city)

compact <- m %>% filter(class == 'compact')
mean(compact$city)

# 7. read.csv(file="samsung.csv"),     finance.yahoo.com 사이트에서 samsung 검색 ->  historical data 단추 -> 2021년 데이터 추출 및 저장(samsung.csv)
# 1) Open, High, Low, Close, Adj.Close, Volume 열을 추출
samsung <- read.csv('F:/data/samsung.csv')

str(samsung)
samsung %>% select(Open)
samsung %>% select(High)
samsung %>% select(Low)
samsung %>% select(Close)
samsung %>% select(Adj.Close)
samsung %>% select(Volume)

# 2) 각 열에 대해 최소값, 최대값, 평균, 표준편차 출력
open <- samsung$Open
open

open_min <- min(open)
open_min
open_max <- max(open)
open_max
open_mean <-mean(samsung$Open)
open_mean
open_sd <- sd(samsung$Open)
open_sd

# 3) 각 열에 대해 정규화 / 표준화
open_normal <- (open - open_min) / (open_max - open_min)
open_normal

open_stand <- (open - open_mean) / open_sd
open_stand

# 4) Low 열과 High 열 각각에 대해 최대 낙차 출력(교재, for문 while문) - 가능하면 하세요!
# - High 열 최대낙차 =  Max(High(n-1)-High(n))       n은 특정일을 의미
#                Low            High
# 2021 1-3    30000
#        1-4    32000
# ...
#        8-31

str(samsung)
samsung
low <- list()
high <- list()

for(i in 1:165){
  low[i] <- abs((samsung$Low[i+1]-samsung$Low[i]))
  high[i] <- abs((samsung$High[i+1]-samsung$High[i]))
}


max(list(1,2,3))







