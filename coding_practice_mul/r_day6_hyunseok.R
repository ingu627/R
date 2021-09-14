# 1. for문으로 다음과 같이 월 이름을 출력
# The month of January
# ...
# The month of December

month = c('January', 'February', 'March', 'April', 'May', 
          'June', 'July', 'August', 'September', 'October',
          'November', 'December')
for(i in 1:length(month)){
  print(paste('The month of', month[i]))
}


# 2. 짝수이면 TRUE, 홀수이면 FALSE를 출력하는 함수 작성.
# 다음 벡터로 테스트하시오.
# c(-5:5)
pr=c(-5:5)

oddTest=function(x){
  oddTrue=ifelse(x%%2 ==0, 'TRUE', 'FALSE') 
  print(oddTrue)
}
oddTest(pr)

# 3. 짝수 개수를 세는 함수 작성.
# 다음 벡터로 테스트하시오.
# c(-5:5)
pr=c(-5:5)

oddCount = function(x){
  oddTrue = ifelse(x%%2==0, 1, 0)
  return (sum(oddTrue))
  
}
oddCount(pr)

# 4. 주어진 숫자가 원주율보다 크면 TRUE, 
# 아니면 FALSE를 출력하는 함수 작성.
# 3과 1:5 벡터에 대해 테스트하시오
pr1=3
pr2=c(1:5)

piTest = function(x){
  test=ifelse(x>pi, 'TRUE', 'FALSE')
  return(test)
}
piTest(pr1)
piTest(pr2)

# 5. 2~99까지 수에 대해
# - 3의 배수에 해당하는 수의 합계를 구하시오.
# - 3의 배수에 해당하는 수의 개수를 구하시오.
pr=c(2:99)
threeTimes=function(x){
  count=ifelse(x%%3==0, 1, 0)
  numberSum=x[x%%3==0]
  print(sum(count))
  print(sum(numberSum))
}
threeTimes(pr)

# 6. 임의의 수 n을 전달받아, n!을 출력하는 함수를 완성하시오. 
# (n>=2, 5!=5*4*3*2*1)
nFactory=1
nFact=function(n){
  if(n>=2){
    for(i in n:1){
      nFactory=nFactory*i
    }
  }
  return(nFactory)
}
nFact(5)

# 7. 반복문을 이용하여 구구단을 출력하시오
gugu=list()
for(i in 2:9){
  for(j in 1:9){
    cat(i,' X ', j, '=', i*j, '\n')
    
  }
  cat('\n')
}


# 8. 반복문을 활용하여 출력하시오
#    *
#   ***
#  *****
# *******

for(i in 1:4){
  for(j in 1:(5-i)){
    cat(" ")
  }
  for(j in 1:((i*2)-1)){
    cat("*")
  }
  cat("\n")
}






