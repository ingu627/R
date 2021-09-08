#### 08-2 ####

## -------------------------------------------------------------------- ##
library(ggplot2)

# aes : x축 displ, y축 hwy로 지정해 배경 생성
ggplot(data = mpg, aes(x = displ, y = hwy))

# 배경에 산점도 추가
# geom_point() : 산점도를 그리는 함수
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

# xlim : x축 범위 3~6으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)

# x축 범위 3~6, y축 범위 10~30으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  xlim(3, 6) + 
  ylim(10, 30)

# 데이터, 축 / 그래프 종류 / 세부 설정

#### 08-3 ####

## -------------------------------------------------------------------- ##
library(dplyr)

# 구동방식별 평균 고속도로 연비
df_mpg <- mpg %>% 
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))

df_mpg

# geom_col() : 막대 그래프를 만드는 함수 (요약표)
ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col()

# reorder() : 막대를 크기 순으로 정렬 가능 
# 정렬 기준 변수 앞에 - 기호를 붙이면 내림차순으로 정렬한다.
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()

# 빈도 막대 그래프 만들기 (원자료)
ggplot(data = mpg, aes(x = drv)) + geom_bar()

ggplot(data = mpg, aes(x = hwy)) + geom_bar()


#### 08-4 ####

## -------------------------------------------------------------------- ##
# geom_line() : 선 그래프 
ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()


#### 08-5 ####

## -------------------------------------------------------------------- ##
# (boxplot)상자 그림 : 집단 간 분포 차이 표현하기
# mpg 데이터의 drv(구동 방식)별 hwy(고속도로 연비)를 상자 그림으로 표현
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()


## -------------------------------------------------------------------- ##
## 1.산점도
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point()

# 축 설정 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  xlim(3, 6) +
  ylim(10, 30)


## 2.평균 막대 그래프

# 1단계.평균표 만들기
df_mpg <- mpg %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy))

# 2단계.그래프 생성하기, 크기순 정렬하기
# geom_col : 막대 그래프 - 요약표
ggplot(data = df_mpg,
       aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + 
  geom_col()


## 3.빈도 막대 그래프
ggplot(data = mpg, aes(x = drv)) + geom_bar()

## 4.선 그래프
ggplot(data = economics, aes(x = date, y = unemploy)) + 
  geom_line()

## 5.상자 그림
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

