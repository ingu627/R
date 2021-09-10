library(dplyr)
# install.packages('dplyr')
library(reshape)
# install.packages('reshape')
library(plyr)

HR = read.csv('./HR_comma_sep.csv')
# for문은 하나의 열에 대해 작동한다. 
# apply는 여러 columns 혹은 row에 대해 동시에 계산할 수 있도록 도와줌
# apply(데이터, 계산 기준(1 혹은 2), 함수)
# 1: 행 별로 계산 / 2: 열 별로 계산
apply(HR[,1:2],2, mean)

colMeans(HR[,1:2])

apply(HR[,1:2],2,sd)

# R에서 연산함수는 na.rm=TRUE 옵션이 존재하지 않으면,
# 모든 결과값이 NA로 나오게 된다.

# 각 변수의 표준편차 구하는 방법 
D = c(1,2,3,4, NA)
E = c(1,2,3,4,5)

DF = data.frame(
  D = D,
  E = E
)
DF
apply(DF,2,sd)

colSd = function(x){
  y= sd(x, na.rm=TRUE)
  return(y)
}
colSd(D)

apply(DF, 2, colSd)

# tapply() : 그룹간 평균을 구하고 싶은 경우 
# tapply(데이터, 그룹, 연산함수)
tapply(HR$satisfaction_level, HR$left, mean)

# lapply() :한번에 여러 변수들에 대해 동일 조건을 주고 싶은 경우

#일반적인 방법 
DF$D2 = gsub(1, 'A', DF$D)
DF$E2 = gsub(1, 'A', DF$E)

DF2 = DF[,1:2]

DF3 = lapply(DF2, function(x) gsub(1,'A',x))
DF3 = as.data.frame(DF3)
DF3

head(rowMeans(HR[,1:2]))

#dplyr 를 썼을 때
# %>% 는 직관적으로 구성이 될 수 있도록 중간다리 역할
HR[,1:2] %>% 
  rowMeans() %>% 
  head()

HR[,1:5] %>% 
  colMeans()

# 데이터 집계 내기
# Summarise
summarise(HR, MEAN=mean(satisfaction_level),
          N = length(satisfaction_level))
# 또는
HR %>% 
  summarise(MEAN = mean(satisfaction_level),
            N = length(satisfaction_level))

HR2_O = ddply(subset(HR, left == 1), c('sales'),
      summarise,
      MEAN = mean(satisfaction_level),
      N = length(satisfaction_level))

HR2_D = HR %>% 
  subset(left ==1) %>% 
  group_by(sales) %>% 
  dplyr::summarise(
    MEAN = mean(satisfaction_level),
    N = length(satisfaction_level)
  )

# 새로운 변수를 추가하고 싶은 경우 
HR3_D = HR2_D %>% 
  mutate(percent = MEAN /N)

# dplyr와 ggplot2의 조합 
library(ggplot2)

HR2_D %>% 
  ggplot() +
  geom_bar(aes(x=sales, y=MEAN, fill=sales), stat='identity') +
  geom_text(aes(x=sales, y= MEAN+0.05,
                label=round(MEAN, 2))) +  # 위에 숫자 표시 
  theme_bw() + #배경화면 
  xlab('부서') + ylab('평균 만족도') + guides(fill = 'none') +
  theme(axis.text.x = element_text(
    angle=45, size = 8.5, color = 'black',
    face='plain', vjust=1, hjust=1))

# 중복데이터 제거하기 및 데이터 프레임 정렬
rep(1:10, 2)
A=rep(1:10, each = 2)

# 중복 제거
unique(A)

# 데이터 프레임에서의 중복 제거 
duplicate = read.csv('./duplicated.csv')
# 전체 중복 제거 
duplicated3_1 = duplicate[-which(duplicated(duplicate)),]

# 변수 한개를 기준으로 중복 제거
# NAME이 같은 변수들 중복 제거 
duplicated3_2 = duplicate[-which(duplicated(duplicate$NAME)),]

# 다변수를 기준으로 중복 제거 

# NAME, ID 두 개의 값이 같은 중복 데이터 제거 
# 변수명으로 제거 
duplicated3_3 = duplicate[!duplicated(duplicate[,c('NAME', 'ID')]),]

# 변수인덱스로 제거 
duplicated3_4 = duplicate[!duplicated(duplicate[,c(2,3)]),]

duplicate$DATE = as.Date(duplicate$DATE, '%Y-%m-%d')
summary(duplicate$DATE)

# as.Date : 년-월-일
# as.Posixct : 년-월-일 시:분:초

# DATE변수 기준으로 정렬 
duplicate_sort = duplicate[order(duplicate[,'DATE'], decreasing = TRUE),]

library(reshape) # 데이터 프레임의 형태를 바꾸고 싶을 때

RESHAPE = read.csv('./reshape.csv')
RESHAPE
CAST_DATA = cast(RESHAPE, OBS + NAME + ID + DATE ~ TEST)

# 다시 원래대로 돌아가는 방법 
MELT_DATA = melt(CAST_DATA, id=c('OBS', 'NAME', 'ID', 'DATE'))
MELT_DATA = na.omit(MELT_DATA)
MELT_DATA

# CAST_DATA의 경우, 그래프 시각화할 때의 데이터 구조로 적합 
# MELT_DATA의 경우, 모델링 할때의 데이터 구조로 적합

# 데이터 합병 
DUPLICATE = duplicate
DUPLICATED3_3 = duplicated3_3
RESHAPE
CAST_DATA

MERGE = merge(DUPLICATED3_3, CAST_DATA[, c(-1,-2,-4)], by='ID',
      all.x = TRUE)
