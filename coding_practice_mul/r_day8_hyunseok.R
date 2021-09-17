library(arules)


data(Epub)
Epub

# 의미있는 규칙들 추출 
# 지지도, 신뢰도, 향상도 확인 
# 많이 빌리는 도서 목록 
# 함께 빌려지는 확률이 높은 도서 목록

inspect(Epub)
summary(itemFrequency(Epub[,]))

itemFrequencyPlot(Epub)
itemFrequencyPlot(Epub, topN=20, type='absolute')

itemFrequencyPlot(Epub, support=0.01)

# Epub2= apriori(Epub, parameter = list(supp=0.001, conf=0.1))
epub.rules= apriori(Epub, parameter = list(support=0.0005, minlen=2))
summary(epub.rules)

lft=inspect(sort(epub.rules, by='lift')[1:50])
con=inspect(sort(epub.rules, by='confidence')[1:50])
sup=inspect(sort(epub.rules, by='support')[1:50])

class(con)

subset(sup, lhs %in% lft$lhs & rhs %in% lft$rhs)




# summary(Epub2)
# attributes(Epub2)
# 
# inspect(head(sort(Epub2, by='lift')))
# 
# Epub3<-subset(Epub2, items %in% 'doc_6e8')
# inspect(Epub3)
# inspect(sort(Epub3, by='lift')[1:5])
