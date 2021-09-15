library(ggplot2)
library(ggthemes)
# install.packages('ggthemes')
# getwd()

HR =read.csv('./HR_comma_sep.csv')
HR$left = as.factor(HR$left)
HR$salary = factor(HR$salary, levels = c('low', 'medium', 'high'))

# classic Theme
ggplot(HR, aes(x=salary)) +
  geom_bar(aes(fill=salary)) +
  theme_classic()

# BW theme
ggplot(HR, aes(x=salary)) +
  geom_bar(aes(fill=salary)) +
  theme_bw()ㅉㅉ

Graph = ggplot(HR,aes(x=salary)) +  
  geom_bar(aes(fill=salary)) 

# 테마 적용
Graph + theme_bw() + ggtitle("Theme_bw") 
Graph + theme_classic() + ggtitle("Theme_classic") 
Graph + theme_dark() + ggtitle("Theme_dark") 
Graph + theme_light() + ggtitle("Theme_light")  

Graph + theme_linedraw() + ggtitle("Theme_linedraw") 
Graph + theme_minimal() + ggtitle("Theme_minimal") 
Graph + theme_test() + ggtitle("Theme_test") 
Graph + theme_void() + ggtitle("Theme_vold") 

# 범례제목 수정
ggplot(HR,aes(x=salary)) +  
  geom_bar(aes(fill=salary)) +
  theme_bw() +
  labs(fill = "범례 제목 수정(fill)")

ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(col = salary)) +
  theme_bw() +
  labs(col = "범례 제목 수정(col)")

# 범례 위치 수정
Graph + theme(legend.position = "top")
Graph + theme(legend.position = "bottom")
Graph + theme(legend.position = c(0.9,0.7))
Graph + theme(legend.position = 'none')

# 범례 테두리 설정
Graph + theme(legend.position = "bottom")


Graph + theme(legend.position = "bottom",
              legend.box.background = element_rect(),
              legend.box.margin = margin(1, 1, 1, 1))

# x축 : 이산형 : scale_x_discrete()
# x축 : 연속형 : scale_x_continuous()
# y축 : 이산형 : scale_y_discrete()
# y축 : 연속형 : scale_y_continuous()

# 축 변경
ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  scale_x_discrete(expand =  c(0,0), labels = c("하","중","상")) +
  scale_y_continuous(expand = c(0,0),breaks = seq(0,8000,by = 1000))

ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  scale_x_discrete(expand =  c(0,0), labels = c("하","중","상")) +
  scale_y_continuous(expand = c(0,0),breaks = seq(0,8000,by = 1000)) +
  scale_fill_discrete(labels = c("하","중","상"))

# 축범위 설정 : 그래프를 표현할 범위를 나타낼 수가 있다.
ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  ylim(0,5000)

ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  ylim(0,13000)

# 색변경
ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  scale_fill_manual(values = c('red','royalblue','tan')) 


ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary), alpha = 0.4) +
  theme_bw() +
  scale_fill_manual(values = c('red','royalblue','tan')) 

# 그래프 분할 및 대칭 이동
ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary), alpha = 0.4) +
  theme_bw() +
  scale_fill_manual(values = c('red','royalblue','tan')) +
  coord_flip()


ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  scale_fill_manual(values = c('red','royalblue','tan'))  +
  guides(fill = FALSE) + 
  facet_wrap(~left,ncol = 1) 

#글자크기, 각도 수정
ggplot(HR,aes(x = salary)) +  
  geom_bar(aes(fill = salary)) +
  theme_bw() +
  scale_fill_manual(values = c('red','royalblue','tan'))  +
  coord_flip() +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 15,angle = 90),
        axis.text.y = element_text(size = 15),
        legend.text = element_text(size = 15))
