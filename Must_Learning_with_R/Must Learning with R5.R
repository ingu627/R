library(dplyr)

STOCK = read.csv('./uniqlo_stocks2012-2016.csv')
STOCK

STOCK$Date = as.Date(STOCK$Date)
STOCK$Year = as.factor(format(STOCK$Date, '%Y'))
STOCK$Day = as.factor(format(STOCK$Date, '%a'))

str(STOCK)

# group_by : 집계 기준 변수를 정해주는 명령어
# summarise : 집계 기준 변수 및 명령어에 따라 요약값을 계산

Group_Data = STOCK %>% 
  group_by(Year, Day) %>% 
  summarise(Mean = round(mean(Open)),
            Median = round(median(Open)),
            Max = round(max(Open)),
            Counts = length(Open)
    
  )

# ungroup : group으로 묶인 데이터를 그룹 해제 시켜주는 명령어 
Ungroup_Data = Group_Data %>% 
  ungroup()

# count : 집계 기준에 따라 데이터의 row 갯수를 계산해준다.
Count_Data = STOCK %>% 
  group_by(Year, Day) %>% 
  count()

# 조건에 따라 데이터 추출하기 (filter(), subset())
Subseted_Data = Group_Data %>% 
  filter(Year == '2012')

head(Subseted_Data)

# distinct() : 데이터 내에 존재하는 중복데이터 제거
SL = sample(1:nrow(Group_Data), 500, replace = TRUE)
Duplicated_Data = Group_Data[SL,]

Duplicated_Data2 = Duplicated_Data %>% 
  distinct(Year,Day,Mean,Median,Max,Counts)

# sample_frac(), sample_n() : 샘플 데이터 무작위 추출
# sample_frac() : 그룹이 지정되어 있는 데이터
Sample_Frac_Gr = Group_Data %>% 
  sample_frac(size=0.4, replace = FALSE)

# 그룹이 해제되어 있는 데이터 
Sample_Frac_Un = Ungroup_Data %>% 
  sample_frac(size=0.4, replace = FALSE)

Sample_N_Gr = Group_Data %>% 
  sample_n(size= 5, replace= FALSE)

Sample_N_Un = Ungroup_Data %>% 
  sample_n(size= 10, replace= FALSE)

# 정해진 index에 따라 데이터 추출하기 
# slice() : index를 직접 설정함으로, 원하는 구간만 추출가능

Slice_Data =Ungroup_Data %>% 
  slice(1:10)

# top_n() : 설정해준 변수를 기준으로 가장 값이 높은 n개의 데이터를 가져온다,
Top_n_Data = Ungroup_Data %>% 
  top_n(5, Mean) # Mean이 가장 높은 5개 데이터 추출

# arrange() : 데이터를 특정 변수를 기준으로 정렬하는 방법

Asce_Data =Ungroup_Data %>% 
  arrange(Mean)
# 내림차순은 변수에 -를 붙여준다.
Desc_Data = Ungroup_Data %>% 
  arrange(-Mean)

# select() : 원하는 변수를 뽑아 낼 수 있다.
# 인덱스, 변수명 다 상관없다.
# index 활용
Select_Data = Group_Data %>% 
  select(1:2)
# Column명 활용
Select_Data = Group_Data %>% 
  select(Year, Day)

# select_if :뽑는 조건을 줄 수 있다.
# Factor 변수만 뽑기
Select_if_Data1 = Group_Data %>% 
  select_if(is.factor)
# integer 변수만 뽑기
Select_if_Data2 = Group_Data %>% 
  select_if(is.integer)


# 새로운 변수 만들기 혹은 한번에 처리하기 
# mutate(): 하나의 변수를 명령어에 따라 추가하는 방법 
Mutate_Data = STOCK %>% 
  mutate(Divided = round(High/Low, 2)) %>% 
  select(Date,High,Low,Divided)

# mutate_if() : 지정해준 모든 변수에 대해 계산식을 적용시켜 준다.
# integer 타입 변수를 모두 numeric으로 변경 
Mutate_If_Data = STOCK %>% 
  mutate_if(is.integer, as.numeric)

# mutate_at() : 지정한 변수들에 대해 계산식을 적용시키는 명령어
Mutate_At_Data = STOCK %>% 
  mutate_at(vars(-Date, -Year, -Day), log) %>% 
  select_if(is.numeric)
