library(stringr)

mytext<-c("software environment","software  environment","software\tenvironment")
mytext
str_split(mytext,' ') #결과는 list로 나옴

# 각 벡터의 길이(단어 개수) (결과 벡터로 출력)
sapply(str_split(mytext, ' '), length)
# 리스트로 출력
lapply(str_split(mytext, ' '), length)

# 문자 개수 
sapply(str_split(mytext, ' '), str_length)

mytext
mytext.nowhitespace=str_replace_all(mytext, "[[:space:]]{1,}"," ")

sapply(str_split(mytext.nowhitespace, ' '), length)
sapply(str_split(mytext.nowhitespace, ' '), str_length)

# 대소문자 통일
mytext <- "The 45th President of the United States, Donald Trump, states that he knows how to play trump with the former president"
mytext
myword<-unlist(str_extract_all(mytext, boundary('word')))
# word = 단어 단위로 묶음
table(myword)
# https://www.rdocumentation.org/packages/searchable/versions/0.3.3.1/topics/boundary
table(tolower(myword))

# 일일이 설정 (고유명사 사전에 설정)
myword <- str_replace(myword, 'Trump', 'Trump_unique_')
myword <- str_replace(myword, 'States', 'States_unique_')

mytext <- c("He is one of statisticians agreeing that R is the No. 1 statistical software.","He is one of statisticians agreeing that R is the No. one statistical software.")
mytext
str_split(mytext, ' ')

# 삭제조건
# 숫자가 최소 1회 이상 연달아 등장 & 그 다음에는 공란이 1회 이상 등장
mytext2 <-str_split(str_replace_all(mytext, "[[:digit:]]{1,}[[:space:]]{1,}", ""), ' ')
mytext2

mytext2[[1]]
str_c(mytext2[[1]], collapse = ' ')

mytext <- "Baek et al. (2014) argued that the state of default-setting is critical for people to protect their own personal privacy on the Internet."
mytext

str_split(mytext, '\\. ')
str_split(mytext, ' ') # 공백문자로 구분

mytext <- c("She is an actor","She is the actor")
mytext

mystopwords <- 'a |an |the' #불용어 사전 정의
str_remove_all(mytext, mystopwords)
# 불용어(stopword): a, an, the, ...
# 코퍼스(말뭉치, corpus)에서 불용어는 제거가 기본

############# tm패키지 ############
# text mining(텍스트 마이닝)

# install.packages('tm')
library(tm)
stopwords('en')
stopwords('SMART')

# 영어와 한국어 차이 
# 영어- 3인칭 주어 -> 동사 형태?, 시제, 명사(단수/복수)
# 한국어 - '가다' -> '가니', '간', '가고' ....
# 어근 동일(stemming)화 작업 : 파생된 단어를 동일하게

# is, was, are, am, were => be

mytext

mystemmer.func <- function(mytextobj){
    mytext<-str_replace_all(mytextobj, '(i|I)s |was |are |am |were ', 'be ')
    mytext
}

mystemmer.func(mytext)

mytext <- c("I am a boy. You are a boy. The person might be a boy. Is Jane a boy?")
mystemmer.func(mytext)

mytext <- "The United States comprises fifty states. In the United States, each state has its own laws. However, federal law overrides state law in the United States."
mytext

myword<- unlist(str_extract_all(mytext, boundary('word')))
table(myword)

mytext.2gram <-str_replace_all(mytext, 'United States', 'United_States') #고유명사 처리
myword2<- unlist(str_extract_all(mytext.2gram, boundary('word')))
table(myword2)
length(table(myword2)) #단어 종류
sum(table(myword2)) #단어 전체 개수

mytext.3gram <-str_replace_all(mytext, '(t|T)he United States', 'United_States') #고유명사 처리
myword3<- unlist(str_extract_all(mytext.3gram, boundary('word')))
table(myword3)
length(table(myword3)) #단어 종류
sum(table(myword3)) #단어 전체 개수

my.text.location <- '../papers/papers'
mypaper<-VCorpus(DirSource(my.text.location))
mypaper #??분야 논문 코퍼스

# VCorpus : 코퍼스 생성 함수 
# DirSource : 텍스트 데이터 저장 폴더 지정

# 도메인(domain)? 데이터 분석 분야 
# 코퍼스? (도메인 관련된) 단어 집합 사전
# ex) 법률 챗봇
# 제가 어제 경범죄 xx 받았는데요 어떻게 벌금을 납부해야 하나요? 

# 챗봇 : ...

# 법률코퍼스에 수록된 단어의 예 : 벌금, 구속, 수배, 범죄 ...

# ex.
# 의료 챗봇
# xxx 아파서 왔어요. 입원을 하려고요. xxx 
# 챗봇 : ...

# => 각 분야의 말할 수 있는 범위가 있다.

summary(mypaper)

# mypaper 말붕치는 리스트 형식
mypaper[[2]]

mypaper[[2]]$content
mypaper[[2]]$meta

meta(mypaper[[2]], tag='author')<- 'Kim'
mypaper[[2]]$meta  

myfunc <- function(x){
  # print(x$content)
  str_extract_all(x$content, "[[:alnum:]]{1,}[[:punct:]]{1}[[:alnum:]]{1,}")
  
}
mypuncts<-lapply(mypaper, myfunc)
table(unlist(mypuncts))

# 수치가 포함된 자료 추출
myfunc <- function(x){
  str_extract_all(x$content, "[[:alpha:]]?[[:digit]]+[[:alpha:]]?")
  
}
mypuncts<-lapply(mypaper, myfunc)
table(unlist(mypuncts))

# tm_map함수 
# tm.map(코펏, 처리작업)
res<-tm_map(mypaper, content_transformer(tolower))

res[[2]]$content
mypaper[[2]]$content

#숫자 제거 
mycorpus <- tm_map(mypaper, removeNumbers)
mycorpus[[2]]$content

# 공백 2개 이상 -> 공백 1개
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus[[2]]$content

# 소문자로 변환 
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus[[2]]$content

# 불용어 제거(mycorpus에서 stopwords('SMART') 모두 제거)
mycorpus <-tm_map(mycorpus, removeWords, words=stopwords('SMART'))


sms_raw<-read.csv('f:/data/sms_spam_ansi.txt')
str(sms_raw)
# 타겟변수는 범주형으로 되어야함
sms_raw$type <- factor(sms_raw$type)
table(sms_raw$type) # 데이터 빈도 확인

# 코퍼스 생성 -> 전처리 -> 스팸메일 분류기 제작(베이지안 필터기)

smscorpus<-VCorpus(VectorSource(sms_raw$text))
smscorpus

print(smscorpus)
inspect(smscorpus)
inspect(smscorpus[1:10])

#내용을 확인하려면 -> smscorpus[[1]]$content와 같음
as.character(smscorpus[[1]])
lapply(smscorpus[1:10], as.character)

#전처리 작업
smscorpus_clean <-tm_map(smscorpus, content_transformer(tolower))
lapply(smscorpus_clean[1:10], as.character)
#숫자 제거 
smscorpus_clean<-tm_map(smscorpus_clean, removeNumbers)
#불용어 제거
smscorpus_clean<-tm_map(smscorpus_clean, removeWords, stopwords())
#특수문자 제거
smscorpus_clean<-tm_map(smscorpus_clean, removePunctuation)

#install.packages('SnowballC')
library(SnowballC)

wordStem(c('learn', 'learned', 'learning', 'learns'))
# 이것도 같음 (어근 추출출)
t<-c('learn', 'learned', 'learning', 'learns')
stemDocument(t)

smscorpus_clean<-tm_map(smscorpus_clean, stemDocument)
smscorpus_clean<-tm_map(smscorpus_clean, stripWhitespace)

lapply(smscorpus_clean[1:3], as.character)

lapply(smscorpus[1:3], as.character)

smsdtm<-DocumentTermMatrix(smscorpus_clean) #DTM
# 문서-단어 행렬
# 문서(5559)*단어(6557) 행렬
# 6557개 : 5559개의 문서가 있는 코퍼스에서 
# 중복을 제외한 유일한 단어의 개수는 6557개임

#          단어1, ...  단어n, 단어n+1, ... Hope .... good ... 단어 6557
# 문서1     0            0        0         1         1
# ...
# 문서5559

smsdtm
#sparse matrix(희소행렬) 


smsdtm_train<-smsdtm[1:4169,] #이메일 제목
smsdtm_test<-smsdtm[4170:5559,]

sms_train_labels<- sms_raw[1:4169,]$type
sms_test_labels<- sms_raw[4170:5559,]$type

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# install.packages('wordcloud')
library(wordcloud)
wordcloud(smscorpus_clean, min.freq = 50) #최소 빈도수가 50이상
#어떤 단어가 많이 등장?
#스팸
#햄
sms_raw[sms_raw$type=='spam',]
# sms_raw %>% filter(type == 'spam') %>% select(text)

spam<-subset(sms_raw, type='spam')
ham<-subset(sms_raw, type='ham')


wordcloud(spam$text, max.words=40)
wordcloud(ham$text, max.words=40)


smsdtm_freq_train<-removeSparseTerms(smsdtm_train, 0.999)
#Sparsity값이 0.999보다 크면 해당 열(단어)은 삭제 
smsdtm_freq_train

findFreqTerms(smsdtm_train, 5) #최소 5번 이상 등장한 단어들

smsfreqwords <- findFreqTerms(smsdtm_train, 5)

smsdtm_freq_train<-smsdtm_train[,smsfreqwords]
smsdtm_freq_test<-smsdtm_test[,smsfreqwords]

# smsdtm_freq_train[[1]]
converts_counts<-function(x){
  x<-ifelse(x>0, 'Yes', 'No')
}

sms_train<-apply(smsdtm_train,2, converts_counts)
sms_test<-apply(smsdtm_test,2, converts_counts)


str(sms_train)
str(sms_test)

# install.packages('e1071')
library(e1071)

sms_classifier <- naiveBayes(sms_train, sms_train_labels, laplace=1)
sms_test_pred<-predict(sms_classifier, sms_test)
sms_test_pred

library(gmodels)
CrossTable(sms_test_pred, sms_test_labels)

# install.packages('gpuR')
# install.packages('devtools')
# library(gpuR)
# library('gpuR')




mtcars
midwest<-as.data.frame(ggplot2::midwest)

#목요일 공부
# 연관규칙(마트 장바구니 데이터셋, 1개월간 수집, 1만건)
# 규칙 발견 -> 추천시스템 
# (올리브, 채소) -> (소스)

#금요일(미니프로젝트)