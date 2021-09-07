iris
plot(iris)

df <- scale(iris[1:4])
summary(df)

df_clusters <- kmeans(df, centers = 3)
df_clusters

df_clusters$cluster
df_clusters$centers
df_clusters$totss

plot(iris[1:4],
     pch=df_clusters$cluster,
     col=df_clusters$cluster)

iris$cluster <- df_clusters$cluster

aggregate(data=iris, Sepal.Length ~ cluster, mean )
aggregate(data=iris, Sepal.Width ~ cluster, mean )
aggregate(data=iris, Petal.Length ~ cluster, mean )
aggregate(data=iris, Petal.Width ~ cluster, mean )


# kmeans => 몇개의 클러스터? 엘보 그래프 -> k+means(k means++)
# k=2,3,...,20

#h-clustering 

#DBSCAN : 반경 x(Eps) 이내에 정의 n(MinPts)개 이상 있으면 
# 하나의 클러스터가 생성
# x : 반경 
# n : 점의 개수 

# 점 p가 반경 x(Eps)이내에 점을 n개 이상 가지고 있다면 -> 클러스터
# -> 이때 p를 클러스터의 중심점(core point)라고 부름


#DBSCAN
# install.packages('fpc')
library(fpc)
library(ggplot2)

data<-data.frame(cbind(c(sample(1:10,30, replace = T),
  sample(20:30,20, replace = T)),
c(sample(1:10,30, replace = T),
  sample(20:30,20, replace = T))))

data
# + 는 계속 코딩을 이어서 한다.

ggplot(data)+
  geom_hline(yintercept = 1:30, colour='grey')+
  geom_vline(xintercept = 1:30, colour='grey')+
  geom_point((aes(x=X1, y=X2)))+
  theme_classic()

db<-dbscan(data, eps=sqrt(18), MinPts = 2)
db
str(db)
db$cluster
db$isseed


# seed = 코어 
# 0 : 노이즈(아웃라이어) (컬럼)

library(RColorBrewer)
ggplot(data)+
  geom_hline(yintercept = 1:30, colour='grey')+
  geom_vline(xintercept = 1:30, colour='grey')+
  geom_point(aes(x=X1, y=X2, col=as.factor(db$cluster)))+
  geom_point(data = data[!db$isseed,], aes(X1,X2), shape=8)+
  theme_bw()

data[!db$isseed,]

#참고 : MinPts(k)값은 데이터의 차원+1로부터 설정해보는게 좋음
#DBSCAN시 적절한 MinPts(k)와 eps값을 결정하는 방법
data<-data.frame(cbind(c(sample(1:10,30, replace = T),
                         sample(20:30,20, replace = T)),
                       c(sample(1:15,30, replace = T),
                         sample(15:30,20, replace = T))))
data

ggplot(data)+
  geom_hline(yintercept = 1:30, colour='grey')+
  geom_vline(xintercept = 1:30, colour='grey')+
  geom_point((aes(x=X1, y=X2)))+
  theme_classic()


disEps <- function(mydata, x, y){
  #mydata: 기준점 
  #x,y : 나머지 점들의 x,y 좌표
  sqrt((mydata[1]-x)^2 + (mydata[2]-y)^2)
}

# 1) 모든 점(데이터)들에 대해 K번째로 가까운 점들과의 거리를 계산 
# ex) 데이터 50건
# 데이터 1번 ~ 50번 각 데이터에 대해 나머지 49개의 데이터와의 거리 계산
# 2) 거리를 기준으로 내림차순 정렬
k<-3
dis<-c()
for(i in 1:nrow(data)){ #i는 1~50까지 값
  dis<-c(dis,sort(apply(data, 1, disEps, x<-data[i,1], y<-data[i,2]))[k+1])
  
}
print(dis) # 50개의 기준점 각각에 대해 4(k+1)번쨰로 가까운 점과의 거리
# nrow(data)


# 3) 그래프를 그리자
# - x축은 점들의 번호
# - y축은 내림차순 정렬된 거리
# 4) elbow로 그려지면, 꺾이는 지점 찾는다.
# -> 찾았다면 이때의 k가 MinPts, y축값이 eps

ggplot()+
  geom_point(aes(x=1:length(dis),
                 y=sort(dis, decreasing = T)))+
  theme_bw()

#MinPts(k값=4), 반경 eps 내에

#MinPts=2, Eps=3.5
db<-dbscan(data, eps=4, MinPts = 4)
ggplot(data)+
  geom_hline(yintercept = 1:30, colour='grey')+
  geom_vline(xintercept = 1:30, colour='grey')+
  geom_point(aes(x=X1, y=X2, col=as.factor(db$cluster)))+
  # geom_point(data = data[!db$isseed,], aes(X1,X2), shape=8)+
  theme_bw()

# iris$Petal.Length, iris$Petal.Width
# -> 시각화
# -> minpts, eps 조정 -> 3개의 클러스터 생성
ggplot(iris)+
  geom_hline(yintercept = 1:10, colour='grey')+
  geom_vline(xintercept = 1:10, colour='grey')+
  geom_point((aes(x=Petal.Length, y=Petal.Width)))+
  theme_classic()


