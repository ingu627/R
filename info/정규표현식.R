# 정규표현식(regex) (regular expression)
# 출처(https://cceeddcc.tistory.com/110)

# 함수 

# grep : pattern을 포함하고 있는 단어의 결과값의 위치를 반환
# grepl : pattern을 포함하고 있는 단어에 대해 T/F 반환
# nchar : character 개수 반환 ,str_length 와 동일
# regexpr : 문자열에서 문자 위치 찾기
# gregexpr : 문자열에서 문자 위치 찾기(global, 전부)
# sub : 문자열에서 문자 바꾸기
# gsub : 문자열에서 문자 바꾸기(global, 전부)
# paste : 띄어쓰기 없이 붙이기

# 정규표현식(regular expression)

# * : 0 or more 
# + : 1 or more 
# ? : 0 or 1 
# . : 무엇이든 한 글자를 의미 
# ^ : 시작 문자 지정 ex) ^[abc] abc중 한 단어 포함한 것으로 시작
# [^] : 해당 문자를 제외한 모든 것 ex) [^abc] a,b,c는 빼고 
# $ : 끝 문자 지정 
# [a-z] : 알파벳 소문자 중 1개
# [A-Z] : 알파벳 대문자 중 1개
# [0-9] : 모든 숫자 중 1개
# [a-zA-Z] : 모든 알파벳 중 1개
# [가-힣] : 모든 한글 중 1개
# [^가-힣] : 모든 한글을 제외한 모든 것

# 실제 사용시에는 [] 한번 더 써야함 ex) [[:digit]]
# [:punct:] : punctuation
# [:alpha:] : letters
# [:lower:] : lowercase letters
# [:upper:] : upperclass letters
# [:digit:] : digits
# [:xdigit:] : hex digits
# [:alnum:] : letters and numbers
# [:cntrl:] : control characters
# [:graph:] : letters, numbers, and punctuation
# [:print:] : letters, numbers, punctuation, and whitespace
# [:space:] : space characters (basically equivalent to \s)
# [:blank:] : space and tab

bananas <- c("banana", "Banana", "BANANA")

# .은 무엇이든 한 글자를 의미함
x <- c("apple", "banana", "pear")
str_extract(x, ".a.")
str_detect("\nX\n", ".X.") # 줄바꿈은 포함 안함
str_detect("\nX\n", regex(".X.", dotall = TRUE)) 
# dotall 은 .이 다음 줄에서도 영향

# grep (globally search a regular expression & print)
char1 <- c('apple','Apple','APPLE','banana','grape')
grep('pp', char1) # pattern을 포함하고 있는 단어의 결과값의 위치를 반환

grepl('pp', char1) # logical 

grep('pp', char1, value=T) # pattern을 포함하고 있는 문자열 반환

# grep()함수에 여러 패턴 사용하기
char2 <- c('apple','banana')
grep(char2,char1)  #패턴을 2개 이상 주면 첫 번째 패턴만 사용

paste(char2,collapse='|')

# regexpr(), gregexpr()
# 문자열에서 문자 위치 찾기 

# grep으로는 위치를 찾을 수 없음.
grep('-','010-8706-4712') 

# 처음 나오는 '-' 문자 위치 찾기 
regexpr('-','010-8706-4712')  

# 나오는 '-' 문자 위치 모두 찾기 , g => global
gregexpr('-','010-8706-4712')  

# sub(), gsub()
# 문자열에서 문자 바꾸기

sub("p","*","apple") # substitute 대체하다 

gsub("p","*","apple") # global
