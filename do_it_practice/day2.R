# 클러스터링? 특성이 비슷한 데이터끼리 묶어주는 머신러닝 기법
# 1. k값을 정해줌 -> k개의 중심점 생성(랜덤)
# 2. 각 중심점과 데이터 사이 거리를 구함 
# 3. 가까운 중심점에 데이터를 할당
# 4. 각 클러스터에 소속된 데이터의 중심점을 구함 
# 5. 중심점 업데이트 -> 2번으로 이동 
# 6. 2-5번과정을 중심점 업데이트가 더이상 발생하지 않을때까지 반복

mean(rnorm(100))

x <- matrix(rnorm(100), nrow=5)
x #5행 20열(5건의 데이터, 20차원 데이터라고 가정하자)
#20차원 공간에 점(데이터)이 5개 있다.

# 첫번째 데이터(점)의 좌표 예시
# (1.0436011,  0.6517660, -0.5624246, -0.05255288, -1.1167117,
#   ... , 0.08652900)


dist(x)

x1 <- c(10,20,1,2,3,4)
y1<-c(5,6,7,8,9,10)
a<-rbind(x1, y1)
a
dist(a)

teens<-read.csv('F:/data/snsdata.csv')
str(teens)
table(teens$gender, useNA = 'ifany')

summary(teens$age)

teens$age<-ifelse(teens$age>=13 & teens$age<20, teens$age, NA)
summary(teens$age)

mean(teens$age)
mean(teens$age, na.rm=TRUE)

#ifelse(teens$gender=='F',1,0)

teens$gender
#NA를 M 또는 F로 대체

#성별 값 자체에 초점을 두고 (NA 빼고)
teens$female <- ifelse(teens$gender=='F' & !is.na(teens$gender),1,0)
table(teens$female) #여성인지 아닌지

teens$no_gender<-ifelse( is.na(teens$gender), 1, 0)
table(teens$no_gender) #NA인지 아닌지

table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

str(teens)


aggregate(data=teens, age ~ gradyear, mean )
#gradyear -> age -> mean

aggregate(data=teens, age ~ gradyear, mean, na.rm=TRUE)
ave_age<-ave(teens$age, teens$gradyear, FUN=function(x) mean(x, na.rm=TRUE))





# 2006 -> 18. XX
# 2007
# 2008
# 2009
teens$age
table(ave_age)

teens$age<-ifelse(is.na(teens$age), ave_age, teens$age)

# 졸업년도 실제나이(age) 친구들의평균나이(avg_age)
# 2006       18.9      18.8
# 2006       18.5      18.8
# 2006       18.1      18.8
# 2006        NA       18.8 
# ...              ...
# 2009       15.3     15.8
summary(teens$age)

head(teens,5)

interests<-teens[5:40]
interests

5개 클러스터 생성

str(interests)
#각 열의 최소값/최대값

summary(interests)

apply(interests, 2, min)
apply(interests, 2, max)

apply(interests, 2, max)-apply(interests, 2, min)


scale(interests)
class(scale(interests))

interests_z<-as.data.frame(lapply(interests, scale))
head(interests_z,5)

summary(interests$basketball)
summary(interests_z$basketball)

set.seed(12345)
teen_clusters<-kmeans(interests_z, 5) #5개 클러스터 생성

teen_clusters


# between_SS : 클러스터간 분산 정도 -> 클수록 클러스터링이 잘 된것임
# within_SS: 클러스터 내 데이터의 분산 정도 -> 작을수록 클러스터링이 잘 된것임


#total_SS =between_SS+within_SS

#(between_SS / total_SS =  11.2 %)


teen_clusters$cluster
teen_clusters$centers
teen_clusters$totss


teens$cluster<-teen_clusters$cluster
head(teens,5)

#insight 추출(클러스터의 특징을 추출)
aggregate(data=teens, age ~ cluster, mean )
#         ( 데이터,  관심있는변수 ~ 그룹화변수, 적용함수)
aggregate(data=teens, female ~ cluster, mean )

aggregate(data=teens, friends ~ cluster, mean )


teens



# 홍길동=(0.75, -0.1, ..., -0.08)
# 클러스터의 중심점(5개)



