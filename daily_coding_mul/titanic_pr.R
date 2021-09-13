train=read.csv('f:/data/titanic/train.csv')
test=read.csv('f:/data/titanic/test.csv')

str(train)
head(train)
str(test)
head(test)

# name, ticket, fare, cabin, passengerId 제거
train=train[,-c(1,4,9,10,11)]
test=test[,-c(1,3,8,9,10)]

str(train$Sex)
table(train$Sex)

library(stringr)
library(magrittr)

train$Sex %<>% gsub('female', 1,.) %<>% gsub('male',0, .)
test$Sex %<>% gsub('female', 1,.) %<>% gsub('male',0, .)

train$Sex = as.integer(train$Sex)
test$Sex = as.integer(test$Sex)

str(train$Embarked)
sum(is.na(train$Embarked))

library(ggplot2)
qplot(train$Embarked)

train$Embarked[train$Embarked ==''] = 'C'

# install.packages("fastDummies")
library(fastDummies)

for_dummy = train[,7]
table(for_dummy)

for_dummy=dummy_cols(for_dummy)[,-1]
train = cbind(train, for_dummy)
train = train[,-7]
train

test$Embarked[test$Embarked ==''] = 'C'

for_dummy = test[,6]
table(for_dummy)

for_dummy=dummy_cols(for_dummy)[,-1]
test = cbind(test, for_dummy)
test = test[,-6]
test

train_age_mean = mean(train$Age, na.rm=TRUE)
train$Age[is.na(train$Age)] = train_age_mean

test_age_mean = mean(test$Age, na.rm=TRUE)
test$Age[is.na(test$Age)] = test_age_mean

head(test$Age)

train$Age = as.integer(train$Age)
test$Age = as.integer(test$Age)

age_level = c()
for(x in 1:length(train$Age)){
  if(train$Age[x]<18){
    age_level[x] ='child'
  }
  else if(train$Age[x]<=50){
    age_level[x] = 'adult'
  }
  else{
    age_level[x] = 'old'
  }
}
head(age_level, 10)

dum_old = dummy_cols(age_level)[-1]
train = cbind(train, dum_old)
train


age_level = c()
for(x in 1:length(test$Age)){
  if(test$Age[x]<18){
    age_level[x] ='child'
  }
  else if(test$Age[x]<=50){
    age_level[x] = 'adult'
  }
  else{
    age_level[x] = 'old'
  }
}
head(age_level, 10)

dum_old = dummy_cols(age_level)[-1]
test = cbind(test, dum_old)
test

normalize=function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

train$Pclass = normalize(train$Pclass)
test$Pclass = normalize(test$Pclass)

train_y = train$Survived

train = train[,-c(1,4)]
test = test[, -c(3)]
str(train)
str(test)
matrix(train)
matrix(test)

train = scale(train)
test = scale(test)

library(class)
titanic_survive = knn(train, test, cl=train_y, k=12)
titanic_survive

# library(gmodels)
# CrossTable(x=test, y=titanic_survive,
#            prop.chisq=F)



submission = read.csv('f:/data/titanic/gender_submission.csv')
submission$Survived = titanic_survive

write.csv(submission, 'submission.csv', row.names = F)

