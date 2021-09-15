# 3) 
# 1. iris 데이터에 대해 knn 적용하여 분류 모델 작성(R언어로 해야함)
# - iris 데이터를 7:3의 비율로 나눈다(sample함수)
# - train 데이터로 knn 모델 생성 (target 컬럼 : species)
# - test 데이터로 테스트 수행 

# * 주의사항
# - 난수 생성시 set.seed(1234)로 설정할 것
# - test 데이터 수행시 정확도 출력

df=iris
df
str(df)

set.seed(1234)
idx =sample(1:nrow(df), size=nrow(df)*0.7 , replace=FALSE)
train = df[idx,]
test = df[-idx,]

length(train$Species)
length(test$Species)

table(train$Species)
table(test$Species)

train_y= train$Species
test_y= test$Species

str(train)
str(test)

train = train[, -c(5)]
test = test[, -c(5)]

train = scale(train)
test = scale(test)

library(class)

knn_21_test = knn(train, test, cl=train_y, k=21)
knn_21_test

test_y

# Confusion Matrix 틀 만들기 
result <- matrix(NA, nrow = 3, ncol = 3) 
rownames(result) <- paste0("real_", levels(train_y)) 
colnames(result) <- paste0("clsf_", levels(train_y))

# Confusion Matrix 값 입력하기 
result[1, 1] <- sum(ifelse(test_y == "setosa" & knn_21_test == "setosa", 1, 0)) 
result[2, 1] <- sum(ifelse(test_y == "versicolor" & knn_21_test == "setosa", 1, 0)) 
result[3, 1] <- sum(ifelse(test_y == "virginica" & knn_21_test == "setosa", 1, 0)) 
result[1, 2] <- sum(ifelse(test_y == "setosa" & knn_21_test == "versicolor", 1, 0)) 
result[2, 2] <- sum(ifelse(test_y == "versicolor" & knn_21_test == "versicolor", 1, 0)) 
result[3, 2] <- sum(ifelse(test_y == "virginica" & knn_21_test == "versicolor", 1, 0)) 
result[1, 3] <- sum(ifelse(test_y == "setosa" & knn_21_test == "virginica", 1, 0)) 
result[2, 3] <- sum(ifelse(test_y == "versicolor" & knn_21_test == "virginica", 1, 0)) 
result[3, 3] <- sum(ifelse(test_y == "virginica" & knn_21_test == "virginica", 1, 0))

result


sum(knn_21_test == test_y) / sum(result)


