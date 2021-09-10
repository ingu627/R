IMDB = read.csv('./imdb-movie-data.csv')

# 결측치 : 데이터에 값이 없는 것
is.na(IMDB$Metascore)[1:20]
sum(is.na(IMDB$Metascore))

colSums(is.na(IMDB))

# na.omit : 결측치가 하나라도 포함된 obs(헁)은 삭제
# 결측치 전부 삭제
IMDB2 =na.omit(IMDB)
colSums(is.na(IMDB2))

# 특정 변수에 결측치가 존재하는 행만 삭제하는 경우
IMDB3 = IMDB[complete.cases(IMDB[, 12]),]
colSums(is.na(IMDB3))

IMDB$Metascore2 =  IMDB$Metascore
IMDB$Metascore2[is.na(IMDB$Metascore2)] =58.99

# 결측치 
# 연속형 변수 : 평균으로 대체 
# 이산형 변수 : 최빈값으로 대체 
# => 결측치의 비율 / 데이터의 분포 / 다른 변수와의 관계가 있는지
mean(IMDB$Revenue..Millions.)

mean(IMDB$Revenue..Millions., na.rm = TRUE)

library(ggplot2)

ggplot(IMDB, aes(x=Revenue..Millions.)) +
  geom_histogram(fill='royalblue', alpha = 0.4) +
  ylab('') +
  xlab('Revenue_Millions') +
  theme_classic()

ggplot(IMDB, aes(x='', y=Revenue..Millions.)) +
  geom_boxplot(fill='red', alha = 0.4, outlier.color = 'red') +
  xlab('') +
  ylab('Revenue_Millions') +
  theme_classic()

summary(IMDB$Revenue..Millions.)
# 평균은 극단값(Outer, 이상치)에 영향을 받는다.
# 중위수는 극단값에 영향을 받지 않는다.

# 이상치 뽑아내기
# 이상치 : 패턴에서 벗어난 값

ggplot(IMDB, aes(x=as.factor(Year), y=Revenue..Millions.)) +
  geom_boxplot(aes(fill=as.factor(Year)), outlier.colour = 'red', alpha=I(0.4))+
  xlab('년도') + ylab('수익') + guides(fill = FALSE) + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))

# Outlier의 처리방법

Q1 = quantile(IMDB$Revenue..Millions., probs = c(0.25), na.rm = TRUE)
Q3 = quantile(IMDB$Revenue..Millions., probs = c(0.75), na.rm = TRUE)

LC = Q1 - 1.5 * (Q3 - Q1)                                   
UC = Q3 + 1.5 * (Q3 + Q1)

IMDB2 = subset(IMDB,
               (Revenue..Millions. > LC)&(Revenue..Millions. < UC) )

# ggplot(IMDB2, aes(x=as.factor(Year), y=Revenue..Millions.))+
  # geom_boxplot(aes(fill=as.factor(Year)), outlier.colour = 'red')

# 문자열 데이터 다루기 
# 문자열 대체 : gsub()
# 문자열 분리 : strsplit()
# 문자열 합치기 : paste()
# 문자열 추출 : substr()
# 텍스트마이닝 함수 : Corpus() & tm_map() & tdm()

#문자열 추출
substr(IMDB$Actors[1], 1, 5)

#문자열 붙이기
paste(IMDB$Actors[1],'_','A')

#띄어쓰기 없이 붙이기
paste(IMDB$Actors[1],'_','A', sep='')

paste(IMDB$Actors[1],'_','A', sep='|')

# paste는 기본적으로 붙이는 문자열 사이에 (한칸 빈칸)이 기본 설정
# 이를 수정하기 위해서는 sep=''옵션을 주어야 한다.

#문자열 분리 
strsplit(as.character(IMDB$Actors[1]), split=',')

#문자열 대체
IMDB$Genre2 = gsub(',', ' ', IMDB$Genre)

# 텍스트 마이닝 
# 1. 코퍼스(말뭉치) 생성
# 2. TDM(문서 행렬) 생성
# 3. 문자 처리(특수문자 제거, 조사 제거, 숫자 제거 등..)
# 4. 문자열 변수 생성

# 1단계: 코퍼스 생성
# 영어의 경우, 대문자와 소문자가 다른 글자로 인식되기 때문에 
# 바꿔주는 작업이 필요 
# install.packages('tm')
library(tm)

# 코퍼스 생성
# Corpus : 말뭉치 : 텍스트 마이닝을 하기 터에
# 문자열 데이터를 정리하는 과정
CORPUS = Corpus(VectorSource(IMDB$Genre2)) 
#특수문자 제거
CORPUS_TM = tm_map(CORPUS, removePunctuation)
#숫자 제거 
CORPUS_TM =tm_map(CORPUS, removeNumbers)
# 알파벳 모두 소문자로 바꾸기 
CORPUS_TM = tm_map(CORPUS_TM, tolower)

# 2단계 : 문서행렬 생성
# => 특정 단어를 변수로 만들어, 분석에 사용  
# => 특정 단어가 포함되어 있는 데이터만 따로 추출하거나
# 특정 단어가 많이 등장하였을 때,
# 이것이 다른 무언과와 상관성이 있는지 분석하기 위한 목적
# 즉, 문자열 데이터를 가지고 통계적인 분석을 하기위한 준비과정

# 문서행렬 생성
TDM = DocumentTermMatrix(CORPUS_TM)
inspect(TDM)

TDM = as.data.frame(as.matrix(TDM))
head(TDM)

# 3단계 : 기존데이터와 결합하기 
IMDB_GENRE = cbind(IMDB, TDM)

# 데이터 결합하기
# cbind : 행이 동일하고, 순서도 같을 때 옆으로 합치기
# rbind : 열이 동일하고, 순서도 같을 때 아래로 합치기
# merge : 열과 행이 다른 두 데이터 셋을 하나의 기준을 잡고 합치고자 할 때 사용 

# Description 변수 사용 (단어의 중복 등장 / 조사, 동사, 명사 등 등장)
# 1단계 : stopwords을 이용한 단어 제거 

library(tm)
CORPUS = Corpus(VectorSource(IMDB$Description))
CORPUS_TM = tm_map(CORPUS, stripWhitespace)
CORPUS_TM = tm_map(CORPUS_TM, removePunctuation)
CORPUS_TM = tm_map(CORPUS_TM, removeNumbers)
CORPUS_TM = tm_map(CORPUS_TM, tolower)

DTM = DocumentTermMatrix(CORPUS_TM)
inspect(DTM)

CORPUS_TM = tm_map(CORPUS_TM, removeWords,
                   c(stopwords('english'), 'my', 'custom', 'words'))

# 중복등장 단어 처리 결정
# 1안 : 특정 단어가 문장에 포함되어 있냐 없냐로 표시 -> 0,1로 코딩(0:포함x, 1:포함 o)
convert_count = function(x){
  y=ifelse(x>0,1,0)
  y = as.numeric(y)
  return(y)
}

# 2안 : 특정 단어가 문장에서 몇번 등장했나를 표시 -> 등장 빈도로 코딩 
convert_count = function(x) {
  y=ifelse(x>0,x,0)
  y=as.numeric(y)
  return(y)
}

DESCRIPT_IMDB = apply(DTM, 2, convert_count)
DESCRIPT_IMDB =  as.data.frame(DESCRIPT_IMDB)

# 3단계 : 문자열 데이터 시각화
# term Document Matrix 생성 
TDM = TermDocumentMatrix(CORPUS_TM)

# 위드 클라우드 생성 
m = as.matrix(TDM)
v =sort(rowSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq=v)

# install.packages("SnowballC")
library('SnowballC')
# install.packages('wordcloud')
library('wordcloud')
# install.packages('RColorBrewer')
library('RColorBrewer')

# min.freq -> 최소 5번 이상 쓰인 단어만 띄우기 
# max.words -> 최대 200개만 띄우기
# random.order -> 단어 위치 랜덤 여부

wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=200, random.order=FALSE,
          colors=brewer.pal(8, 'Dark2'))

# 단어 빈도 그래프 그리기
ggplot(d[1:10,]) +
  geom_bar(aes(x = reorder(word, freq), y=freq),
           stat = 'identity') +
  coord_flip() + xlab('word') + ylab('freq') +
  theme_bw()
