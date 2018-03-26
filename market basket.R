library(arules)

dataset<-read.csv('Market_Basket_Optimisation.csv',header = FALSE)

View(dataset)

dataset= read.transactions('Market_Basket_Optimisation.csv', sep=',',rm.duplicates = TRUE)
summary(dataset)


itemFrequencyPlot(dataset,topN = 20)

#training the apriori on the dataset

rules= apriori(data = dataset, parameter = list(support = 0.004,confidence = 0.2))


#visualizing the results

inspect(sort(rules, by='lift')[1:10])
