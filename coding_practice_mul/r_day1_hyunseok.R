# Q1. 시험 점수 변수 만들고 출력하기
# 다섯 명의 학생이 시험을 봤습니다. 학생 다섯 명의 시험 점수를 담고 있는 변수를 만들어 출력해 보세요. 각
# 학생의 시험 점수는 다음과 같습니다.
# 80, 60, 70, 50, 90
score <- c(80, 60, 70, 50, 90)
score

# Q2. 전체 평균 구하기
# 앞 문제에서 만든 변수를 이용해서 이 학생들의 전체 평균 점수를 구해보세요.
mean(score)

# Q3. 전체 평균 변수 만들고 출력하기
# 전체 평균 점수를 담고 있는 새 변수를 만들어 출력해 보세요. 앞 문제를 풀 때 사용한 코드를 응용하면
# 됩니다.
stu_mean <- mean(score)
stu_mean


# ggplot2 패키지에는 미국 동북중부 437개 지역의 인구통계 정보를 담은 midwest라는 데이터가 포함되어
# 있습니다. midwest 데이터를 사용해 데이터 분석 문제를 해결해보세요.
library(ggplot2)
library(dplyr)
# • 문제 1. ggplot2 의 midwest 데이터를 데이터 프레임 형태로 불러와서 데이터의 특성을 파악하세요.
midwest
head(midwest)
tail(midwest)
View(midwest)
dim(midwest)
str(midwest)
summary(midwest)

# • 문제 2. poptotal(전체 인구)을 total 로, popasian(아시아 인구)을 asian 으로 변수명을 수정하세요.
midwest <- rename(midwest, total = poptotal , asian = popasian)
midwest

# • 문제 3. total, asian 변수를 이용해 '전체 인구 대비 아시아 인구 백분율' 파생변수를 만들고, 히스토그램을
# 만들어 도시들이 어떻게 분포하는지 살펴보세요.
percent_asian <- midwest$asian / midwest$total
hist(percent_asian)

# • 문제 4. 아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 "large", 그 외에는 "small"을 부여하는
# 파생변수를 만들어 보세요.
percent_asian_mean <- sum(midwest$asian) / sum(midwest$total)
midwest$LS <- ifelse(percent_asian > percent_asian_mean, 'large', 'small')
midwest$LS

# • 문제 5. "large"와 "small"에 해당하는 지역이 얼마나 되는지, 빈도표와 빈도 막대 그래프를 만들어 확인해
# 보세요.
qplot(data= midwest, x=LS)

# *시간 있는 분들은~
# 소스코드 3,4,5 강 연습
