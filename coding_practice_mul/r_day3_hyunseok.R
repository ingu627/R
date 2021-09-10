# iris
# 150건 데이터
# 1~100번까지 데이터 추출 ->   Petal.Length Petal.Width 열 저장
# -> 회귀모델 : 독립변수(Petal.Length), 종속변수(Petal.Width)
# -> 회귀모델의 기울기, 절편 구해지면
# 
# 101번~150번까지 데이터 추출 ->   Petal.Length Petal.Width 열 저장
# -> 이미 구한 회귀모델의 기울기, 절편을 이용하여 Petal.Length를
# 입력했을때 Petal.Width 예측하여 출력
# 
# cost도 함께 출력(예측값 - 실제값) 제곱의 평균

rss1 <- function(data){
  print(str(data))
  x<-data[,3][1:100]
  y<-data[,4][1:100]
  res<-lm(y~x)
  print(res)
  HX<- coefficients(res)[1] + coefficients(res)[2]*x
  result <- sum((y-HX)^2)
  print(HX)
  print(result)
  
}

rss2 <- function(data){
  print(str(data))
  x<-data[,3][101:150]
  y<-data[,4][101:150]
  print(y)
  res<-lm(y~x)
  HX<- coefficients(res)[1] + coefficients(res)[2]*x
  result <- sum((y-HX)^2)
  print(HX)
  
}


rss1(iris) # cost값
rss2(iris)
