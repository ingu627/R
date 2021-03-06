#### 04-2 ####

## ---------------------------------------------------------------------- ##

english <- c(90, 80, 60, 70)  # 영어 점수 변수 생성
english

math <- c(50, 60, 100, 20)    # 수학 점수 변수 생성
math

# english, math로 데이터 프레임 생성해서 df_midterm에 할당
df_midterm <- data.frame(english, math)
df_midterm

str(df_midterm) #파이썬에서 info()와 비슷
# obs = observation (관측치)
# 데이터프레임 변수명$컬럼명 <==> # df['컬럼명']
# ex) df_midterm$english

class <- c(1, 1, 2, 2)
class

df_midterm <- data.frame(english, math, class)
df_midterm

mean(df_midterm$english)  # df_midterm의 english로 평균 산출
mean(df_midterm$math)     # df_midterm의 math로 평균 산술

df_midterm <- data.frame(english = c(90, 80, 60, 70),
                         math = c(50, 60, 100, 20),
                         class = c(1, 1, 2, 2))
df_midterm

str(df_midterm)
# str = structure (==파이썬의 info)

# data.frame(addr=c('seoul','busan','daegu','incheon'),
#            age=c(25,35,45,30),
#            job=c('student', 'teacher', 'ds', 'housewife'))


#### 04-3 ####

## -------------------------------------------------------------------- ##
# install.packages("readxl")
library(readxl)

df_exam <- read_excel("./data/excel_exam.xlsx")  # 엑셀 파일을 불러와서 df_exam에 할당
df_exam                                   # 출력

mean(df_exam$english)
mean(df_exam$science)


df_exam_novar <- read_excel("./data/excel_exam_novar.xlsx")
df_exam_novar

df_exam_novar <- read_excel("./data/excel_exam_novar.xlsx", col_names = F)
df_exam_novar

# 엑셀 파일의 세 번째 시트에 있는 데이터 불러오기
df_exam_sheet <- read_excel("./data/excel_exam_sheet.xlsx", sheet = 3)
df_exam_sheet


## -------------------------------------------------------------------- ##
df_csv_exam <- read.csv("./data/csv_exam.csv")
df_csv_exam


## -------------------------------------------------------------------- ##
df_midterm <- data.frame(english = c(90, 80, 60, 70),
                         math = c(50, 60, 100, 20),
                         class = c(1, 1, 2, 2))
df_midterm
str(df_midterm)

write.csv(df_midterm, file = "df_midterm.csv")
# 파일 생성
# csv 는 범용적인 데이터파일

## -------------------------------------------------------------------- ##
save(df_midterm, file = "df_midterm.rda")
# rda = r전용 데이터

rm(df_midterm)
# rm = 메모리 상에서 제거 (remove)

load("df_midterm.rda")
# 파일 불러오기

## -------------------------------------------------------------------- ##
# 1.변수 만들기, 데이터 프레임 만들기
english <- c(90, 80, 60, 70)  # 영어 점수 변수 생성
math <- c(50, 60, 100, 20)    # 수학 점수 변수 생성
data.frame(english, math)     # 데이터 프레임 생성

# 2. 외부 데이터 이용하기

# 엑셀 파일
library(readxl)                                 # readxl 패키지 로드
df_exam <- read_excel("excel_exam.xlsx")        # 엑셀 파일 불러오기

# CSV 파일
df_csv_exam <- read.csv("csv_exam.csv")         # CSV 파일 불러오기
write.csv(df_midterm, file = "df_midterm.csv")  # CSV 파일로 저장하기

# Rda 파일
load("df_midterm.rda")                          # Rda 파일 불러오기
save(df_midterm, file = "df_midterm.rda")       # Rda 파일로 저장하기

