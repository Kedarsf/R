# Decision Tree Classification
getwd()
setwd("C:/Users/Admin/Desktop/Tharangini/PGP_DSP/Decision trees")
# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
#dataset = dataset[3:5]
View(dataset)
# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased)
dataset<-dataset[,-1]
colSums(is.na(dataset))
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
#set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
View(test_set)
#univariate analysis

# Feature Scaling
training_set[,c(2,3)] = scale(training_set[,c(2,3)])
test_set[,c(2,3)] = scale(test_set[,c(2,3)])
#test_set[-3] = scale(test_set[-3])
help(rpart)
# Fitting Decision Tree Classification to the Training set
# install.packages('rpart')
library(rpart)
classifier = rpart(formula = Purchased ~ .,
                   data = training_set)
help(rpart)
# Validating using the validation set 
y_pred = predict(classifier, newdata = test_set[,-4], type = 'class')

# Making the Confusion Matrix
cm = table(y_pred,test_set$Purchased)
acc(cm)
 
#prediction for the prediction set
plot(classifier,uniform=TRUE,cex=0.8)
text(classifier, use.n=TRUE, all=TRUE)
