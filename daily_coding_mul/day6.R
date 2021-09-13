myvector<-c(1:6,'a')
myvector

mylist<-list(1:6,'a')
mylist

형태소 -> 단어 -> 문장 -> 문단 -> 문서..

obj1<-1:4
obj2<-6:10
class(obj1)
obj3<-list(obj1,obj2)
obj3

mylist<-list(obj1,obj2,obj3)
mylist

#[[]] 사용하여 리스트에 저장된 요소를 접근할 수 있다
#mylist[[1]]은 obj1을 접근
mylist[[3]]

mylist[[3]][[2]]
mylist[[3]][2]


mylist[[3]]


mylist
#벡터자료 접근 : [], 리스트 자료 접근 : [[]]
mylist[[3]][[1]][3]

#unlist:리스트 -> 벡터

myvector<-c(1:6,'a')
mylist<-list(1:6,'a')

myvector==unlist(mylist)

name1<-"Donald"
myspace<-" "
name2<-"Trump"
list(name1, myspace, name2)
unlist(list(name1, myspace, name2))

#"Donald_Trump"

#메타데이터(데이터의 데이터) : 데이터에 대한 설명
#ex. 유튜브영상(데이터)에 달린 캡션,설명,제목,작성자...

name<-c("갑","을","병","정")
gender<-c(2,1,1,2)
mydata<-data.frame(name,gender)
attr(mydata$name, "var")<-"이름"
mydata$name

attr(mydata$gender, "var")<-"성별"

myvalues<-gender
for(i in 1:length(mydata$gender)){
  myvalues[i]<-ifelse(gender[i]==1,"남성","여성")
}
myvalues

#속성값 저장
attr(mydata$gender,"value")<-myvalues
mydata$gender

#속성값 추출
attr(mydata$gender,"value")

# 축구 골 넣는 영상 검색
# 기존 검색 : 유튜브 검색어로 '골인' -> 검색 결과(영상 제목, 메타데이터)
# 미래 검색 : 유튜브 검색어로 '골인' -> 검색 결과(영상 이해 -> 메타데이터 생성)

mylist<-list(1:4, 6:10, list(1:4,6:10))
mylist

lapply(mylist[[3]],mean)

lapply(mylist, mean)

lapply(mylist[c(1,2)],mean)

#sapply : lapply 결과에 대해 unlist를 적용
lapply(mylist[c(1,2)],sum)
unlist(lapply(mylist[c(1,2)],sum))

sapply(mylist[c(1,2)],sum)


letters[22]
LETTERS

tolower("Hi")
toupper("Hi")

nchar("Korea")
nchar("한국")

nchar("Korea", type='bytes')
nchar("한국", type='bytes')

mysentence<-"Learning R is so interesting"
#문장 -> 단어
strsplit(mysentence, split=" ")
#리스트의 첫번째 요소가 벡터, 벡터 내부에는 단어들이 요소값으로 저장
unlist(strsplit(mysentence, split=" "))


mywords<-strsplit(mysentence, split=" ")
mywords[[1]]

strsplit(mywords[[1]][5], split="")

myletters<-list(rep(NA,5))
#length(mywords[[1]])
#mywords에 저장된 모든 단어들에 대해 문자로 분해하시오.
for (i in 1:length(mywords[[1]])){
  myletters[i]<-strsplit(mywords[[1]][i], split="")
}
myletters
#list(rep(NA,5))

paste(myletters[[1]], collapse = '')

mywords[[1]]
strsplit(mywords[[1]], split="")



paste(myletters[[1]],collapse = '')
paste(myletters[[2]],collapse = '')
paste(myletters[[5]],collapse = '')

mywords2<-list(rep(NA,5))
for(i in 1:5){
  mywords2[i]<-paste(myletters[[i]],collapse = '')
}
mywords2

R_wiki <- "R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."

R_wiki

#문단 단위로 분리
rWikiPara<-strsplit(R_wiki, split="\n")
rWikiPara #리스트의 길이? 1(길이가 2인 벡터 1개가 들어가 있음)
#문장 단위로 분리

strsplit(R_wiki, split="\\.") #전체 문장(R_wiki)을 문장 단위로 분리
#리스트의 길이? 1(길이가 7인 벡터 1개가 들어가 있음)

#strsplit(rWikiPara, split="\\.") #리스트 이므로 에러 발생

rWikiPara
rWikiSent<-strsplit(rWikiPara[[1]], split="\\.")
#벡터의 요소들을 대상으로 문장 단위로 분리
rWikiSent
#length(rWikiSent)   문단 2 개
#length(rWikiSent[[1]])  첫번째 문단에는 문장이 3개
length(rWikiSent[[2]])  #두번째 문단에는 문장이 4개


rWikiWord<-list(NA,NA)

for(i in 1:2){
  rWikiWord[[i]]<-strsplit(rWikiSent[[i]], split=" ")
}
rWikiWord

rWikiWord[[1]][[2]][3] #문단번호 문장번호 단어번호

mysentence<-"Learning R is so interesting"
regexpr("ing", mysentence) #정규표현식

#패턴 매칭된 문자열의 시작위치 리턴
loc.begin<-as.vector(regexpr("ing", mysentence)) #6

loc.length<-attr(regexpr("ing", mysentence), "match.length") #3

#매치된 문자열의 종료위치
loc.end<-loc.begin + loc.length -1
loc.end

mysentence

#전체 데이터에 대해 매칭
gregexpr("ing", mysentence)

gregexpr("ing", mysentence)[[1]]

length(gregexpr("ing", mysentence)[[1]])


loc.begin<-as.vector(gregexpr("ing", mysentence)[[1]])


mysentences<-unlist(rWikiSent)

mysentences

regexpr("software", mysentences)

mytemp<-regexpr("software", mysentences)

my.begin<-as.vector(mytemp)
my.begin[my.begin==-1]<-NA
my.begin

my.end<-attr(mytemp, "match.length") + my.begin -1


mylocs<-matrix(NA, nrow=length(my.begin), ncol=2)
mylocs

colnames(mylocs)<-c("begin", "end")
# mylocs
# colnames(mylocs)

paste("sentence", 1:length(my.begin), sep=".")
mylocs

rownames(mylocs)<-paste("sentence", 1:length(my.begin), sep=".")
mylocs

for(i in 1: length(my.begin)){
  mylocs[i,]<-cbind(my.begin[i], my.end[i])
}
mylocs

grep("software", mysentences)
mysentences

grepl("software", mysentences)

#문자열 치환
sub('ing', 'ING', mysentence)
mysentence

gsub('ing', 'ING', mysentence)

sent1<-rWikiSent[[1]][1]
sent1

new.sent1<-gsub("R Foundation for Statistical Computing",
     "R_Foundation_for_Statistical_Computing",
     sent1)
new.sent1

sum(table(strsplit(sent1,split=" ")))
sum(table(strsplit(new.sent1,split=" ")))

#전치사 제거
drop.sent1<-gsub("and |the |for |the ", "", new.sent1)
sum(table(strsplit(drop.sent1,split=" ")))

regmatches()

mysentence
mypattern<-regexpr("ing",mysentence)
regmatches(mysentence, mypattern)

mypattern<-gregexpr("ing",mysentence)
regmatches(mysentence, mypattern)


mypattern<-regexpr("ing",mysentence)
regmatches(mysentence, mypattern, invert = TRUE)

mypattern<-gregexpr("ing",mysentence)
regmatches(mysentence, mypattern, invert = TRUE)

mysentence
strsplit(mysentence, split="ing")

gsub("ing", "", mysentence)

mysentences
substr(mysentences, 1, 10)

my2sentence <- c("Learning R is so interesting","He is a fascinating singer")
my2sentence

#ing로 끝나는 모든 단어를 검색(Learning, interesting, fascinating)
mypattern0<-gregexpr("ing", my2sentence)
regmatches(my2sentence, mypattern0)

#help -> regex 검색
mypattern0<-gregexpr("[[:alpha:]]+ing\\b", my2sentence) #  ing\\b: ing로 끝남
regmatches(my2sentence, mypattern0)

myexample<-"He (Obama) received the Nobel Prize"
mypattern1<-gregexpr("\\([[:alpha:]]+\\)", myexample)
regmatches(myexample, mypattern1)

#소문자로 통일
mysentences
mypattern<-gregexpr("[[:alpha:]]+ing\\b", mysentences)
regmatches(tolower(mysentences), mypattern)
table(unlist(regmatches(tolower(mysentences), mypattern)))

#대소문자 추출
mypattern<-gregexpr("[[:upper:]]", mysentences)
table(unlist(regmatches(mysentences, mypattern)))




mypattern<-gregexpr("[[:lower:]]", mysentences)
table(unlist(regmatches(mysentences, mypattern)))


table(unlist(regmatches(mysentences, mypattern)))


#가장 많이 쓰인 소문자는?
mypattern<-gregexpr("[[:lower:]]", mysentences)
lowercha<-table(unlist(regmatches(mysentences, mypattern)))
lowercha

lowercha[lowercha==max(lowercha)]
#몇가지 종류의 알파벳 소문자가 쓰였을까?
length(lowercha)
# 총 몇 개의 알파벳 소문자가 쓰였을까?
sum(lowercha)

# endsWith()
# x1 <- c("Foobar", "bla bla", "something", "another", "blu", "brown",
#         "blau blüht der Enzian")
# 
# endsWith(x1, "n")
# startsWith(x1, "bla")

library(stringr)

R_wiki <- "R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."

str_extract(R_wiki, "software environment")
str_extract_all(R_wiki, "software environment")
str_extract_all(R_wiki, "software environment", simplify = TRUE) #행렬 

#첫글자가 대문자인 모든 단어들을 조사
myextract<-str_extract_all(R_wiki, "[[:upper:]]{1}[[:alpha:]]{0,}")
table(myextract)

str_extract(R_wiki, "software environment")
str_match(R_wiki, "software environment")

str_extract_all(R_wiki, "software environment")
str_match_all(R_wiki, "software environment")

str_match(R_wiki, "[[:alpha:]]{1,}")
str_match_all(R_wiki, "[[:alpha:]]{1,} and [[:alpha:]]{1,}")

university_address <- c("연세대학교 주소는 서울시 서대문구 연세로 50번지다",
                        "서울대 주소: 서울시 관악구 관악로 1번지다",
                        "고려대는 서울시 성북구 안암로 145번지에 있다",
                        "카이스트 주소, 대전시 유성구 대학로 291번지",
                        "포항시 남구 청암로 77번지는 포항공과 대학교 주소임")

#시 구 로 번지 모두 추출
style_address <- "([[:alpha:]]{1,}시) ([[:alpha:]]{1,}구) ([[:alpha:]]{1,}로) ([[:digit:]]{1,}번지)"
str_match(university_address, style_address)


myaddress<-data.frame(str_match(university_address, style_address))
myaddress
names(myaddress) <- c("full_address","city","district","road","street")
myaddress
#myaddress[,3]

table(myaddress$city)

rWikiPara<-str_split(R_wiki,"\n")
rWikiPara

rWikiSent<-str_split(rWikiPara[[1]], "\\. ")
rWikiSent

my2sentences<-unlist(rWikiSent)[c(4,7)]
my2sentences
#두 문장의 단어수를 각각 출력하시오

length(unlist(str_split(my2sentences[1], " ")))
length(unlist(str_split(my2sentences[2], " ")))





