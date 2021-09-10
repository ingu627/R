# install.packages('foreign')
library(foreign)
library(readxl)
library(dplyr)

welfare<-read.spss('f:/data/koweps_hpc10_2015_beta1.sav',
                   to.data.frame=T)
class(welfare)

str(welfare)

welfare <- rename(welfare,
                  sex = h10_g3, # 성별
                  birth = h10_g4, # 태어난 연도
                  marriage = h10_g10, # 혼인 상태
                  religion = h10_g11, # 종교
                  income = p1002_8aq1, # 월급
                  code_job = h10_eco9, # 직종 코드
                  code_region = h10_reg7) # 지역 코드
                  #age를
welfare$sex

table(welfare$sex)

welfare$birth
welfare$marriage

# 1. 성별
# 성별에 따른 급여 차이 등 분석 & 시각화

welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean(income))

# 나이에 따른 급여 차이 등 분석 & 시각화(나이 대 형성)
# 혼인 상태 분석(혼인 비율, 이혼율, ...), 혼인 상태와 급여 관계
# 종교와 급여 관계, 종교와 이혼율 관계 분석
# 지역별 연령대 비율표
# 직종코드 기반 급여를 많이 받거나 적게 받는 직종 30개씩 검색
# 직종코드는 있지만, 급여가 확인되지 않은 3000명 정도의 NA가 존재
# 급여가 NA인 사람의 직종코드를 참조하여, 동일한 직종코드의 해당되는
# 다른 사람들의 데이터를 찾아서 급여를 유추하여 결측치 대체

table(welfare$religion)

welfare$code_job

library(readxl)
read_excel('f:/data/Koweps_Codebook.xlsx')
read_excel('f:/data/Koweps_Codebook.xlsx', sheet = 2)

sum(is.na(welfare$income))
sum(is.na(welfare$code_job))
