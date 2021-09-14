# 1. mtcars데이터 weight열 추가, 무게가 중위수보다 큰 자동차는 heavy, 그렇지 않은 자동차는 light를 저장
# - 각 종류별 데이터 건수 출력, 비율
mtcars
View(mtcars)
str(mtcars)
med= median(mtcars$wt)
mtcars$weight = ifelse(mtcars$wt > med, 'heavy', 'light')
mtcars$weight= as.factor(mtcars$weight)
library(ggplot2)
ggplot(mtcars, aes(x=weight)) +
  geom_bar(aes(fill=weight), alpha=I(0.7))

prop.table(table(mtcars$weight))


# 2. ggplot2 패키지에는 미국 동북중부 437개 지역의 인구통계 정보를 담은 midwest라는 데이터가 포함되어 있음. 
# midwest 데이터를 사용하여,
# 불러오기 : midwest<-as.data.frame(ggplot2::midwest)
midwest<-as.data.frame(midwest)
midwest

# - ggplot2 의 midwest 데이터를 데이터 프레임 형태로 불러와서 데이터의 특성을 파악
str(midwest)
dim(midwest)
summary(midwest)

# - poptotal(전체 인구)을 total 로, popasian(아시아 인구)을 asian 으로 변수명을 수정
midwest1 = midwest
library(dplyr)
midwest1 = rename(midwest1, total = poptotal)
midwest1 = rename(midwest1, asian = popasian)

# - total, asian 변수를 이용해 '전체 인구 대비 아시아 인구 백분율' 파생변수 생성
midwest1$total
midwest1$asian


midwest1$percent_of_asian = midwest1$asian / midwest1$total
hist(midwest1$percent_of_asian)

# - 아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 "large", 그 외에는 "small"을 부여하는 파생변수 생성
mean_asian = mean(midwest1$percent_of_asian)
mean_asian

midwest1$large_or_small= ifelse(midwest1$percent_of_asian > mean_asian, 'large', 'small')

# - "large"와 "small"에 해당하는 지역이 얼마나 되는지 빈도수를 출력
ggplot(midwest1, aes(x=factor(large_or_small))) +
  geom_bar(aes(fill=factor(large_or_small))) + 
  theme_bw() +
  xlab('평균 대비 아시안 비율') + ylab('갯수')


# 3. 타이타닉 데이터 분석
# - 타이타닉 데이터 불러오기
titanic= read.csv('f:/data/titanic/train.csv')

head(titanic)
str(titanic)

# - 생존자 수, 사망자 수 출력
table(titanic$Survived)

ggplot(titanic, aes(x=Survived)) +
  geom_bar(aes(fill=factor(Survived)))


# - pclass, embarked 별 승객수 출력(비율)
titanic$Pcount = titanic$PassengerId
titanic$Pcount = 1
titanic$Pcount
total=sum(titanic$Pcount)

titanic %>% 
  group_by(Pclass, Embarked) %>% 
  summarise(countP = sum(Pcount)/total)

# - Name에서 호칭 종류 출력, 호칭 종류 별 승객수 출력

# - 호칭을 아래와 같이 변경하여 name2열에 추가
# * "Mlle", "Ms", "Lady", "Dona" 는 "Miss"로 변경
# * "Mme"는  "Mrs"로 변경
# * "Capt", "Col", "Major", "Dr", "Rev", "Don",  "Sir", "the Countess", "Jonkheer"는 "Officer"로 변경
# * "Mr", "Mrs", "Miss"는 그대로
# * 나머지 호칭은 "Others"
titanic$Name
library(stringr)
titanic$name1= str_extract(titanic$Name, ' [a-zA-Z]+\\.')
titanic$name1

titanic$name1= str_replace(titanic$name1, ' ', '')
titanic$name1= str_replace(titanic$name1, '\\.', '')
titanic$name1

titanic$name1= str_replace(titanic$name1, 'Mlle|Ms|Lady|Dona', 'Miss')
titanic$name1= str_replace(titanic$name1, 'Mme', 'Mrs')
titanic$name1= str_replace(titanic$name1, 'Capt|Col|Major|Dr|Rev|Don|Sir|the Countess|Jonkheer', 'Officer')

titanic$name1= str_replace(titanic$name1, 'Master|Countess', 'Others')
titanic$name1

# -name2 열을 factor로(5가지 범주) 변환
table(titanic$name1)
titanic$name1= as.factor(titanic$name1)


# -name2열의 호칭별 인원수 출력
ggplot(titanic, aes(x=name1)) +
  geom_bar(aes(fill=name1))

# -호칭 정보를 바탕으로 나이(Age) 결측값 대체
# (호칭 별 나이의 평균값)
str(titanic$Age)
sum(is.na(titanic$Age))
colSums(is.na(titanic))


name_age= titanic %>% 
  group_by(name1) %>% 
  summarise(mean_age = mean(Age, na.rm=T))
name_age  

titanic$Age[(is.na(titanic$Age)) & (titanic$name1=='Miss')] = name_age$mean_age[1]
titanic$Age[(is.na(titanic$Age)) & (titanic$name1=='Mr')] = name_age$mean_age[2]
titanic$Age[(is.na(titanic$Age)) & (titanic$name1=='Mrs')] = name_age$mean_age[3]
titanic$Age[(is.na(titanic$Age)) & (titanic$name1=='Officer')] = name_age$mean_age[4]
titanic$Age[(is.na(titanic$Age)) & (titanic$name1=='Others')] = name_age$mean_age[5]


# -age열의 구간별 인원수 출력
titanic %>% 
  group_by(name1) %>% 
  summarise(mean_age = mean(Age),
            total_count = sum(Pcount))

ggplot(titanic, aes(x=name1, y=Age)) +
  geom_boxplot(aes(x=name1, y=Age))


# 10대 미만, 10대, 20대, 30대, 40대, 50대 이상
titanic$age_cat= ifelse(titanic$Age > 50, '50대이상',
       ifelse(titanic$Age > 40, '40대',
              ifelse(titanic$Age > 30, '30대',
                     ifelse(titanic$Age > 20, '20대', '10대'))))

ggplot(titanic, aes(x=factor(age_cat))) +
  geom_bar(aes(fill=factor(age_cat)))


# -cabin 컬럼의 1번째 글자 출력(NA는 제외)
titanic$Cabin
sum(is.na(titanic$Cabin))

substr(titanic$Cabin, 1, 1)

# - fare열 값에 대해 최대/최소/평균/표준편차 출력
str(titanic)

max(titanic$Fare)
min(titanic$Fare)
mean(titanic$Fare)
sd(titanic$Fare)

# - sibsp + parch를 더하여 새롭게 family열에 저장
titanic$family = titanic$SibSp + titanic$Parch
titanic$family




