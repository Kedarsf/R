dataset<-read.csv('Social_Network_Ads.csv')
dataset<-dataset[2:5]
View(dataset)

colSums(is.na(dataset))
is.factor(dataset$Purchased)
dataset$Purchased<-as.factor(dataset$Purchased)
class(dataset$Purchased)
#scaling the data

dataset[,c(2,3)]<-scale(dataset[,c(2,3)])
View(dataset)
#splitting the data
library(caTools)
set.seed(18)
split<-sample.split(dataset$Purchased,SplitRatio = 0.75)

train<-subset(dataset,split==T)
test<-subset(dataset,split==F)
View(test)

#building the model

library(e1071)

classifier<-naiveBayes(Purchased~.,data = train)

#predicting the model

ypred<-predict(classifier,newdata = test[-4])

View(ypred)

#confusion matrix

cm<-table(ypred,test[,4])

cm
#accuracy of model
acc(cm)
