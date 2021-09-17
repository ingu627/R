# *미니 프로젝트 주제*
#   Market_Basket_Optimisation 데이터를 분석하여 마트의 수익을 최대로 하기 위한 
# 다양한 전략을 세워보세요. (시각화도 함께 해주세요)
# ex) 상품 배치, 묶음 판매, 특정 상품에 대한 판매 전략 등

library(readxl)
library(arules)
library(ggplot2)

basket_data = read.transactions('f:/data/Market_Basket_Optimisation.csv',
                  format = 'basket',
                  sep = ',')

summary(basket_data)
# most frequent items: mineral water(1788) eggs(1348)  spaghetti(1306)  french fries(1282)     chocolate(1229)

inspect(basket_data)

# 상위 item : mineral water, eggs, spaghetti, french fries...
sort(itemFrequency(basket_data, type='absolute'), decreasing=TRUE) # 판매건수
itemFrequencyPlot(basket_data, topN=20, type='absolute') # 상위 20개 시각화화


# apriori() : 규칙들을 생성하는 함수
basket_apr= apriori(basket_data, parameter = list(supp=0.008,
                                      conf=0.25,
                                      target = 'rules',
                                      maxlen = 2))
summary(basket_apr)


inspect(basket_apr[1:10]) # mineral water가 가장 많이 언급됨

lft=inspect(sort(basket_apr, by='lift')[1:50])
con=inspect(sort(basket_apr, by='confidence')[1:50])
sup=inspect(sort(basket_apr, by='support')[1:50])

subset(sup, lhs %in% lft$lhs & rhs %in% lft$rhs)


# mineral water
basket_apr_mw <- apriori(basket_data, parameter = list(supp = 0.008, conf = 0.25), 
                              appearance = list(default = "lhs", rhs = "mineral water")) 

rules_mw <- sort(basket_apr_mw, by = "confidence", decreasing = TRUE)
inspect(rules_mw)


inspect(head(sort(basket_apr, by="lift"))) 
#향상도 기준으로 
# {milk, soup} -> {mineral water}
# {frozen vegetables,ground beef} -> {mineral water}
# {eggs,ground beef} -> {mineral water}
# 동시에 사는 품목들 


# eggs
basket_apr_eggs <- apriori(basket_data, parameter = list(supp = 0.008, conf = 0.25), 
                         appearance = list(default = "lhs", rhs = "eggs")) 

rules_eggs <- sort(basket_apr_eggs, by = "confidence", decreasing = TRUE)
inspect(rules_eggs)
#향상도 기준으로 
# {burgers,french fries} -> {eggs}
# {burgers} -> {eggs}
# {turkey} -> {eggs}
# {chocolate,milk} -> {eggs}
# {milk,mineral water} -> {eggs}


# spaghetti
basket_apr_spaghetti <- apriori(basket_data, parameter = list(supp = 0.008, conf = 0.25), 
                           appearance = list(default = "lhs", rhs = "spaghetti")) 

rules_spaghetti <- sort(basket_apr_spaghetti, by = "confidence", decreasing = TRUE)
inspect(rules_spaghetti)
#향상도 기준으로 
# {frozen vegetables, ground beef} ->{spaghetti}
# {eggs, ground beef} ->{spaghetti}
# {ground beef, milk} ->{spaghetti}
# {ground beef, mineral water} ->{spaghetti}



# install.packages('arulesViz')
library(arulesViz)


# mineral water에 대한 시각화
plot(rules_mw, method = 'graph')

plot(rules_mw, method = 'paracoord')

plot(rules_mw, method='grouped matrix')

# eggs에 대한 시각화
plot(rules_eggs, method = 'graph')

plot(rules_eggs, method = 'paracoord')

plot(rules_eggs, method='grouped matrix')

# spaghetti에 대한 시각화
plot(rules_spaghetti, method = 'graph')

plot(rules_spaghetti, method = 'paracoord')

plot(rules_spaghetti, method='grouped matrix')



