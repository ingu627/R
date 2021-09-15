library(ggplot2)
library(dplyr)

STOCK = read.csv('./Uniqlo.csv')

STOCK$Date = as.Date(STOCK$Date)
STOCK$Year = as.factor(format(STOCK$Date, '%Y'))
STOCK$Day = as.factor(format(STOCK$Date, '%a'))

Group_Data = STOCK %>% 
  group_by(Year, Day) %>% 
  summarise(Mean = round(mean(Open)),
            Median = round(median(Open)),
            Max = round(max(Open)),
            Counts = length(Open))
Group_Data

# Bar Chart

# 막대도표는 가장 기본적인 그래프. 
# 하나의 이산형 변수에 대해 시각화를 하는 그래프. 
# 기본적으로 y축은 따로 설정할 필요는 없다.

# 하나의 이산형 변수를 기준으로 x축 변수 1개로만 그리는 경우
ggplot(Group_Data) +
  geom_bar(aes(x = as.factor(Counts), fill = ..count..)) +
  xlab('') + ylab('') +
  scale_fill_gradient(low = '#CCE5FF', high = '#FF00FF') +
  theme_classic() + ggtitle('Continuous Color')
# scale_fill_gradient()를 통해서 칸의 크기와 
# count의 최저,최대치를 설정 가능

ggplot(Group_Data) +
  geom_bar(aes(x = as.factor(Counts), fill = Day), alpha =.4) + 
  # fill 함수는 도형에 색을 채워줄 때 사용
  # bar그래프와 오차그래프 등 채울 공간이 있는 도형에 색상을 채운다.
  # <-> color 함수는 선의 색깔을 채워줄 때 사용하는 함수
  # Bar 그래프의 테두리, Line그래프의 색상, 
  # 경우에 따라 Point그래프의 Point 테두리 등에 색상을 채워줌 
  xlab('') + ylab('')+
  theme_classic() + ggtitle('Discrete Color')
  