# install.packages('arules')
library(arules)

# read.csv('f:/data/groceries.csv')

groceries=read.transactions('f:/data/groceries.csv', sep = ',')
#9835행 169열= 9835건의 거래, 169개 상품

summary(groceries)

groceries
inspect(groceries)

inspect(groceries[1:10])

itemFrequency(groceries)
itemFrequency(groceries, type='absolute') # 판매건수

groceries[,1:3]
itemFrequency(groceries[,1:3])

itemFrequencyPlot(groceries)
itemFrequencyPlot(groceries, type='absolute')


#가장 많이 팔린 20개 상품 시각화
itemFrequencyPlot(groceries, topN=20, type='absolute')

#지지도가 0.1이상인 상품 시각화
itemFrequencyPlot(groceries, support=0.1)

mat=matrix(c(1,1,1,0,0,
         
         1,1,1,1,0,
         
         0,0,1,1,0,
         
         0,1,0,1,1,
         
         0,0,0,1,0), ncol=5, byrow=TRUE)
mat

# paste('row', 1:5)
rownames(mat)<-paste0('row', 1:5)
mat

colnames(mat)<- letters[1:5]
# letters[1:5]

mat
#row는 거래 데이터(장바구니)
#col는 아이템 이름
str(mat)
class(mat)

#행렬 데이터를 transactions 데이터로 변환
mat.trans<-as(mat, 'transactions')
mat.trans
summary(mat.trans)

mat

inspect(mat.trans)

#데이터프레임 데이터를 transactions 데이터로 변환 
df<- as.data.frame(mat)

df.trans<-as(df, 'transactions')
# 경고가 나오는 이유는 데이터프레임이 수치데이터로 구성되어 있어 발생

str(df)

#수치(numeric) 데이터가 저장된 데이터프레임-> logical 데이터로 변환된 데이터프레임
# class(sapply(df, as.logical))
df<- as.data.frame(sapply(df, as.logical))
df.trans<-as(df, 'transactions')

df.trans
summary(df.trans)
inspect(df.trans)

# str(df.trans)
# inspect(df.trans)

#list 데이터를 transactions 데이터로 변환 
mylist <- list(row1=c("a","b","c"),
             row2=c("a","d"),
             row3=c("b","e"),
             row4=c("a","d","e"),
             row5=c("b","c","d"))
mylist
mylist.trans<-as(mylist, 'transactions')
summary(mylist.trans)
inspect(mylist.trans)

##################################################
#groceries 데이터 연관 규칙 생성 
groceryrules<- apriori(groceries, parameter = list(support=0.006, confidence=0.25))

groceryrules

inspect(groceryrules[300:350])

inspect(groceryrules[1:10])
#if lhs then rhs

summary(groceryrules)

inspect(groceryrules[1:10])

inspect(sort(groceryrules, by='lift')[1:30])

#관심있는 상품에 대한 연관규칙을 검색
# ex. beef와 함께 묶어서 판매할만한 상품?
beefrules<-subset(groceryrules, items %in% 'beef')
inspect(beefrules)
inspect(sort(beefrules, by='lift')[1:5])

write(beefrules, file='beefrules.csv', sep=',', row.names=FALSE, quote=TRUE)

groceryrules_df<-as(groceryrules, 'data.frame')
groceryrules_df


data(Epub)
Epub

# 의미있는 규칙들 추출 
# 지지도, 신뢰도, 향상도 확인 
# 많이 빌리는 도서 목록 
# 함께 빌려지는 확류이 높은 도서 목록