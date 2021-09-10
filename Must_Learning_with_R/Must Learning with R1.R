x1 = c(1:10)
x1
# seq = sequence 
# seq(시작숫자, 마지막 숫자, 증가범위)
x1_2 = seq(1,10,1)
x1_2
x2 = seq(1,10,2)
x2

# rep = repeat 
# rep() : rep(반복할 값, 반복할 횟수)
y = rep(1,10)
y
y2 = rep(c(1,10),2)
y2
y3 = rep(c(1,100), c(2,2))
y3

#matrix(data=데이터, nrow=행의 수, ncol=열의 수, byrow=행/열 기준)
MATRIX_R =matrix(
  data = x1,
  nrow = 5
)

MATRIX_C = matrix(
  data = x1,
  ncol = 5
)
MATRIX_C

DATA_SET = data.frame(
  x1 = x1, # 변수명 = 벡터값
  x1_2 = x1_2,
  x2 = x2,
  y=y
)
DATA_SET

# 1차원 벡터일 경우
# 벡터에 속한 원소의 갯수
length(x1)

# 2차원 행렬
dim(MATRIX_R)

dim(DATA_SET)

# c() : 들어오는 값들을 묶어 하나의 벡터로 만드는 기능을 실행
# ()안에는 분석하고자 하는 원소값들이 입력되어야 한다.
a=c(1,2,3,4,5)
a

# {}는 for, if문 등에서 조건식을 삽입할 때 쓰인다.
for(i in a){
  print(a)
}

B = c() # 빈 공간의 벡터 생성

for(k in seq(1,10,2)){
  B=c(B,k)
}
B

# []는 index를 입력해야 될 때 쓰인다.
# 1, 2번째 값
a[1:2]
# 3번째 값 빼고
a[-3]

a[c(1,2,4,5)]

# 2차원 data.frame()형태의 경우

# 1행 전부
DATA_SET[1,]

# 1열 전부
DATA_SET[,1]

# 1,2,3행 & 2열 빼고 나머지
DATA_SET[c(1,2,3),-2]

# chr : 문자열 형태
# int : 숫자
# num : 숫자
# Factor : 명목형 변수
# Posixct : 시간 변수
# Tseries : 시계열 변수 

Numeric_Vector = c(1:20)
Chr_Vector = c('A','B','C')
str(Numeric_Vector)
str(Chr_Vector)

# as.Date(변수, format='날짜 형식')
DATE_O = '2018-01-02'
DATE_C = as.Date(DATE_O, format = '%Y-%m-%d')
DATE_C

# as.POSIXct(날짜, format = '날짜형식')
DATE_02 = '2015-02-04 23:13:23'
DATE_P = as.POSIXct(DATE_02, format = '%Y-%m-%d %H:%M:%S')
DATE_P

# format(날짜변수, '형식)
format(DATE_P, '%A')
format(DATE_P, '%M')
format(DATE_P, '%Y')

# as() : 변수 x를 ~ 취급하겠다
x=c(1,2,3,4,5,6,7,8,9,10)
x1 = as.integer(x) ;x1
x2 = as.numeric(x) ; x2
x3 = as.factor(x) ; x3
x4 = as.character(x) ; x4

summary(x1)

# is : 논리문으로써 변수 x가 ~인지 판단하여라라는 의미 
# str = Strings()

# sample(데이터 추출범위, 데이터 추 갯수, replace = )
S1 =sample(1:45, 6, replace= FALSE)

# set.seed() : 결과값 고정
set.seed(1234)
S2 = sample(1:45, 6, replace=FALSE)
print(S2)

# %in% ~에 속해 있는지 확인
A = c(1,2,3,4,5)

if(7 %in% A){
  print('TRUE')
} else{
  print('FALSE')
}

# function()을 통해 사용자함수 만들기 
Plus_One = function(x){
  y= x+1
  return (y)
}
Plus_One(3)

# install.packages('ggplot2')
library(ggplot2)
