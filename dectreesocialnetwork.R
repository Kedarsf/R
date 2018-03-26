getwd()

dc<-read.csv("C:/Users/Admin/Documents/Social_Network_Ads.csv")

View(dc)

summary(dc)

colSums(is.na(dc))

class(dc$Purchased)

#converting the purchase into factor

dc$Purchased<-factor(dc$Purchased)

dc$User.ID<-NULL

library(caTools)

# splitting the data

split = sample.split(dc$Purchased, SplitRatio = 0.75)
trainset = subset(dc, split == TRUE)
testset = subset(dc, split == FALSE)
View(testset)

# feature scaling

trainset[,c(2,3)] = scale(trainset[,c(2,3)])
testset[,c(2,3)] = scale(testset[,c(2,3)])

#installing repart

library(rpart)
classifier = rpart( Purchased ~ .,data = trainset)
# validating the validation set

ypred<-predict(classifier, newdata = testset[,-4], type = 'class')

# confusion matrix

cm = table(ypred,testset$Purchased)

acc(cm)

#prediction for the prediction set
plot(classifier,uniform=TRUE,cex=0.8)
text(classifier, use.n=TRUE, all=TRUE)
