train= read.csv('f:/data/titanic/train.csv')
test= read.csv('f:/data/titanic/test.csv')

str(train)
train=train[,-c(4,9,11)]
str(test)
test=test[,-c(3,8,10)]

train$Sex= ifelse(train$Sex == 'male', 1, 0)
test$Sex= ifelse(test$Sex == 'male', 1, 0)

table(train$Embarked)
sum(is.na(train$Embarked))

train$Embarked[train$Embarked=="",] = 'C'

#승선 항구별 Fare의 평균 출력
library(dplyr)
train %>% 
  group_by(Embarked) %>%  
  summarise(mean_Embarked = mean(Fare))

# 또는
aggregate(train$Fare, by=list(train$Embarked), FUN=mean)
aggregate(data=train, Fare ~ Embarked, mean)
# 또는
tapply(train$Fare, train$Embarked, mean)

train$Embarked = ifelse(train$Embarked == 'C', 0,
                        ifelse(train$Embarked=='Q',1,2))
test$Embarked = ifelse(test$Embarked == 'C', 0,
                        ifelse(test$Embarked=='Q',1,2))

#na를 평균나이로 대체
sum(is.na(train$Age))

train_age=na.omit(train$Age)
train_age_avg = mean(train_age)
train$Age[is.na(train$Age)]=train_age_avg

test_age=na.omit(test$Age) #결측치 제거
test_age_avg = mean(test_age)
test$Age[is.na(test$Age)]=test_age_avg

train_age=na.omit(train$Age)
train_age_avg = mean(train_age)
train$Age[is.na(train$Age)]=train_age_avg

train$Age <- ifelse(train$Age < 18, 1, 0)
test$Age <- ifelse(test$Age < 18, 1, 0)

# survived=train$Survived
# passengers=test$PassengerId
#library(class)
# res=knn(train, test, survived, k=5)
# submission=data.frame(PassengerId=passengers, Survived=reset_theme_settings()
# submission
# write.csv(submission, 'submission.csv', row.names = FALSE)

