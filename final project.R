
#FINAL PROJECT 
# checking the working ditrectory 
getwd()

# assigning the name to the datset
project<-read.csv('C:/Users/Admin/Documents/bank (1).csv',na.string = c(""," ","NA"))

#viewing the dataset
View(project)

# checking for the missing values 
colSums(is.na(project))

#checking the diminsions of dataset
dim(project)

# removing the unique columns 
project$pdays<-NULL

# imputation
# mode function 

mode <- function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

project$job[is.na(project$job)]<-mode(project$job)
project$marital[is.na(project$marital)]<-mode(project$marital)
project$education[is.na(project$education)]<-mode(project$education)
project$default[is.na(project$default)]<-mode(project$default)
project$housing[is.na(project$housing)]<-mode(project$housing)
project$loan[is.na(project$loan)]<-mode(project$loan)

#chwcking for NA's in main dataset 
colSums(is.na(project))

# creating the levels

levels(project$job)<-1:11
levels(project$marital)<-1:3
levels(project$education)<-1:7
levels(project$default)<-1:2
levels(project$housing)<-1:2
levels(project$loan)<-1:2
levels(project$contact)<-1:2
levels(project$month)<-1:10
levels(project$day_of_week)<-1:5
levels(project$poutcome)<-1:3
levels(project$y)<-0:1

# checking the class of the dependent variable 
class(project$y)

# checking for the outliers 
boxplot(project)

# scaling the dataset 

project$age<-scale(project$age)
project$duration<-scale(project$duration)
project$cons.price.idx<-scale(project$cons.price.idx)
project$cons.conf.idx<-scale(project$cons.conf.idx)
project$euribor3m<-scale(project$euribor3m)
project$nr.employed<-scale(project$nr.employed)

# splitting of the dataset into train and validate set 

library(caTools)
set.seed(100)
split<- sample.split(project$y , SplitRatio = 0.70)
train_set<-subset(project,split ==TRUE)
val_set<-subset(project,split ==FALSE)
View(val_set)

# building the model on train set and validating it on validation set 
attach(train_set)

# 1 Logistic regression model 

model <-glm(y~ . , family = binomial(link = 'logit'), data = train_set)
summary(model)  #aic = 11959

model1<-glm(y~job+education+contact+month+day_of_week+duration+campaign+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx , family = binomial(link = 'logit') , data = train_set)
summary(model1) # aic = 11974

# prediction 
y_pred<-predict(model1 , newdata = val_set[-20])
y_pred = ifelse(y_pred>=0.5,1,0)
table(y_pred)
write.csv(y_pred,file ="Final prediction.csv")
# confusion matrix 
cm<-table(y_pred, val_set$y)
cm

# accuracy 
acc<- function(cm){
  tp<-cm[2,2]
  fp<-cm[2,1]
  acc<-tp/(tp+fp)
  acc
}
acc(cm) # accuracy = 69.39 


# building the auc curve

#library(ROCR)
#pr<-prediction(y_pred,val_set$y)
#pr
#prf<-performance(pr,measure = "tpr",x.measure = "fpr")
#obtaining area under ROC curve
#auc<-performance(pr,measure = "auc")
#auc
#auc1<-auc@y.values[[1]]
#auc1
#plot(prf) # auc is 64.37%

# 2 Decision Tree

library(rpart)
classifier <- rpart(formula = y~., data = train_set)

# prediction 
y_pred2<-predict(classifier , newdata = val_set[-20] , type = 'class')


# confusion matrix
cm2 = table(y_pred2 , val_set$ y)
cm2

# accuracy 
acc(cm2)  # accuracy of DT is 61.86


# 3 SVM

library(e1071)
set.seed(123)
classifier1<-svm(formula = y~.,data = train_set, type = 'C-classification' ,kernel = 'radial')
classifier2<-svm(formula = y~.,data = train_set, type = 'C-classification' ,kernel = 'sigmoid')
classifier3<-svm(formula = y~.,data = train_set, type = 'C-classification' ,kernel = 'polynomial')

# prediction 
y_pred3<-predict(classifier1, newdata = val_set[-20])
y_pred4<-predict(classifier2, newdata = val_set[-20])
y_pred5<-predict(classifier3, newdata = val_set[-20])

# confusion matrix 
cm3<-table(y_pred3,val_set$y)
cm3

cm4<-table(y_pred4,val_set$y)
cm4

cm5<-table(y_pred5,val_set$y)
cm5

# accuracy 
acc(cm3) # accuracy is 67.88
acc(cm4) # accuracy 45.48
acc(cm5) # accuracy =73.35


# 4 Naive Bayes classifier 

set.seed(5465)
classifier4<-naiveBayes(y~.,data = train_set)

# prediction 
y_pred6<-predict(classifier4,newdata = val_set[-20])

#confusion matrix
cm6<-table(y_pred6,val_set$y)
cm6

# accuracy 
acc(cm6) #accuracy = 36.95% 


#5. knn 

library(class)
y_pred7 <-knn(train = train_set,test = val_set,cl=train_set$y,k=300,prob = TRUE)

#confusion matrix
cm7<-table(y_pred7,val_set$y)
cm7

#accuracy
acc(cm7) #accuracy = 88.73%

# 6 Random Forest

set.seed(1234)
library(randomForest)
classifier4 = randomForest(x=train_set[-20], y = train_set$y, ntree = 200)

# prediction 
y_pred8<-predict(classifier4,newdata = val_set[-20] )

# confusion matrix
cm8<- table(y_pred8,val_set$y)
cm8

# accuracy
acc(cm8)  # accuracy = 63.53%

# loading the test data 
test_set<-read.csv('C:/Users/Admin/Documents/bank-additional (1).csv',na.string = c(""," ","NA"))

#viewing the test_set
View(test_set)

#checking the NA's in the test_set
colSums(is.na(test_set))

#checking the dimensions of test_set
dim(test_set)

# checking for the outliers 
boxplot(test_set)

# scaling the dataset 
test_set$duration<-scale(test_set$duration)
test_set$age<-scale(test_set$age)
test_set$cons.price.idx<-scale(test_set$cons.price.idx)
test_set$cons.conf.idx<-scale(test_set$cons.conf.idx)
test_set$euribor3m<-scale(test_set$euribor3m)
test_set$nr.employed<-scale(test_set$nr.employed)

# converting data into levels 

levels(test_set$job)<-1:11
levels(test_set$marital)<-1:3
levels(test_set$education)<-1:7
levels(test_set$default)<-1:2
levels(test_set$housing)<-1:2
levels(test_set$loan)<-1:2
levels(test_set$contact)<-1:2
levels(test_set$month)<-1:10
levels(test_set$day_of_week)<-1:5
levels(test_set$poutcome)<-1:3


# prediction on test data 

result <-knn(train = train_set[-20],test = test_set,cl=train_set$y,k=300,prob = TRUE)
test_set<-data.frame(test_set,result)
summary(test_set)
summary(result)


#################################-----------###############################

# 1.	Which machine learning approach is appropriate to find the 
#solution for the above mentioned problem?

# We found out that knn is the best machine learning approach for this dataset.

#2.	Predict the term deposit subscription for the Bank additional dataset 
#and conclude if the telemarketing campaign was a success or not.

# After we run summary on the result variable we came to know that there are
# only 16 people out of 3090 people who secured the term deposit .Thus we can
#conclude that the telemarketing campaign was an utter failure.

#3.	What is are the key differentiators 
#between the ones who have subscribed (Yes) and who did not (No).

#As we have used knn machine learning algorith we actually don't know which 
#are the significant variables effecting the prediction process.












































