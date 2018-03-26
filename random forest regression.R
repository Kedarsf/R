pos<-read.csv("Position_Salaries.csv")

summary(pos)

colSums(is.na(pos))

View(pos)

pos$Level<-NULL

#bivariate analysis

aov(pos$Salary~pos$Position)

# splitting the data into train and test set

library(caTools)
set.seed(456)
split=sample.split(pos,SplitRatio = 0.75)
train<-subset(pos,split==T)
test<-subset(pos,split==F)
View(train)
# random forest

library(randomForest)

regressor<-randomForest(x=train[-2],y = train$Salary, ntree = 500)

ypred<-predict(regressor,newdata = test[-2])

# rmse 
library(hydroGOF)
rmse1<-rmse(test$Salary,ypred)
rmse1
