getwd()
dataset<-read.csv("C:/Users/Admin/Documents/Python_Module_Day_16.2_Network_Intrusion_Train_data.csv")

View(dataset)


# data processing 
class(dataset$class)
summary(dataset)
colSums(is.na(dataset))
#removing irrelavent columns
dataset$is_host_login<-NULL
dataset$num_outbound_cmds<-NULL
#Convert nominal variables to numeric codes such as flag, protocol type, service

levels(dataset$service)<-1:70
levels(dataset$flag)<-1:11
levels(dataset$protocol_type)<-1:3
#levels(dataset$class)<-1:2
summary(dataset)
 # normalising the data
normalise <- function(dataset, na.rm = TRUE){
  ranx <- range(dataset, na.rm = T)
  (dataset - ranx[1]) / diff(ranx)
}
apply(dataset[,c(1,5,6,10,13,16,17,21,22,30,31)],2,normalise)
#install.packages('rpart')
library(rpart)

model1<-glm(class~.,family = binomial(link = 'logit'),data = dataset)
summary(model1)

model2<-glm(class~duration+service+flag+src_bytes+dst_bytes+land+hot+num_failed_logins+logged_in+num_compromised+num_root+is_guest_login+count+srv_count+serror_rate+srv_serror_rate+rerror_rate+srv_rerror_rate+same_srv_rate+diff_srv_rate+srv_diff_host_rate+dst_host_count+dst_host_srv_count+dst_host_same_srv_rate+dst_host_diff_srv_rate+dst_host_same_src_port_rate+dst_host_srv_diff_host_rate+dst_host_serror_rate+dst_host_srv_rerror_rate+dst_host_srv_serror_rate+dst_host_rerror_rate,family = binomial(link = 'logit'),data = dataset)
summary(model2)

model3<-glm(class~duration+service+flag+src_bytes+dst_bytes+land+hot+num_failed_logins+logged_in+num_compromised+num_root+is_guest_login+count+srv_count+serror_rate+srv_serror_rate+rerror_rate+srv_rerror_rate+same_srv_rate+diff_srv_rate+srv_diff_host_rate+dst_host_count+dst_host_srv_count+dst_host_same_srv_rate+dst_host_same_src_port_rate+dst_host_srv_diff_host_rate+dst_host_srv_serror_rate+dst_host_rerror_rate,family = binomial(link = 'logit'),data = dataset)
summary(model3)

# we will choose model 3 

#reading the validation data set

dataset2<-read.csv("Python_Module_Day_16.4_Network_Intrusion_Validate_data.csv")
summary(dataset2)
View(dataset2)
# data processing 
class(dataset2$class)
summary(dataset2)
colSums(is.na(dataset2))
#dataset2ind<-dataset2[,-40]
#View(dataset2ind)
#dataset2dep<-dataset2$class
#View(dataset2dep)
#removing irrelavent columns
dataset2$num_outbound_cmds<-NULL
dataset2$is_host_login<-NULL
#Convert nominal variables to numeric codes such as flag, protocol type, service

levels(dataset2$service)<-1:70

levels(dataset2$flag)<-1:11
levels(dataset2$protocol_type)<-1:3
#levels(dataset2$class)<-1:2


summary(dataset2)
# normalising the data
normalise <- function(dataset2, na.rm = TRUE){
  ranx <- range(dataset2, na.rm = T)
  (dataset2 - ranx[1]) / diff(ranx)
}
  apply(dataset2[,c(1,5,6,10,13,16,17,21,22,30,31)],2,normalise)
  #validating our model using validationset
  ypred <- predict(model3,newdata=dataset2,type='response')
  ypred <- ifelse(ypred>=0.5,1,0)
  summary(ypred)
  
  
  #confusion matrix
  
  cm<-table(ypred,dataset2$class)
  
  cm
  #accuracy function
  acc<-function(cm){
    Totp<-cm[2,1]+cm[2,2]
    TP<-cm[2,2]
    c<-TP/Totp
    c
  }
  
  acc(cm)
  
  #testing the validated model on the test data
  
  dataset3<-read.csv("Python_Module_Day_16.3_Network_Intrusion_Test_data.csv")

 View(dataset3)
 
 colSums(is.na(dataset3))
 
 #removing irrelavent columns
 dataset3$num_outbound_cmds<-NULL
 
 dataset3$is_host_login<-NULL
 #Convert nominal variables to numeric codes such as flag, protocol type, service
 
 levels(dataset3$service)<-1:70
 
 levels(dataset3$flag)<-1:11
 levels(dataset3$protocol_type)<-1:3
 #levels(dataset3$class)<-1:2
 summary(dataset3)
 
 
 # normalising the data
 normalise <- function(dataset3, na.rm = TRUE){
   ranx <- range(dataset3, na.rm = T)
   (dataset3 - ranx[1]) / diff(ranx)
 }
 apply(dataset3[,c(1,5,6,10,13,16,17,21,22,30,31)],2,normalise)
 #validating our model using validationset
 class <- predict(model3,newdata=dataset3,type='response')
 class <- ifelse(class>=0.5,"normal","anomaly")
 summary(class) 
 
 dataset3<-cbind(dataset3,class)
View(dataset3) 
library(ROCR)
#plotting auc curve

pr<- prediction(ypred,dataset2$class)
prf<-performance(pr,measure="tpr",x.measure="fpr")
auc10<- performance(pr,measure="auc")
auc10<-auc10@y.values[[1]]
auc10
plot(prf)

#decision tree for train data
ClassifierD<-rpart(formula=class~.,
                   data=dataset)

#decision tree for test dataset
#decision tree
plot(ClassifierD,uniform=TRUE,cex=0.8)
text(ClassifierD, use.n=TRUE, all=TRUE)

#Calculating accuracy using DT
y_predD =predict(ClassifierD, newdata=dataset2, type = 'class')
y_predD<- ifelse(y_predD=="normal",1,0)
CFD= table(y_predD,dataset2$class)

AccuracyD<-acc(CFD)
AccuracyD

library(ROCR)
#plotting auc curve

pr_DT<- prediction(y_predD,dataset2$class)
prf<-performance(pr,measure="tpr",x.measure="fpr")
auc<- performance(pr,measure="auc")
auc2<-auc@y.values[[1]]
auc2
plot(prf)

# support vector machine

library(e1071)

classifier1<-svm(formula=class~.,data = dataset,type='C-classification',kernel='linear')
classifier2<-svm(formula=class~.,data = dataset,type='C-classification',kernel='radial')
classifier3<-svm(formula=class~.,data = dataset,type='C-classification',kernel='sigmoid')
classifier4<-svm(formula=class~.,data = dataset,type='C-classification',kernel='polynomial')



#Calculating accuracy using SVM
y_pred1 = predict(classifier1, newdata =dataset2)
y_pred1<- ifelse(y_pred1=="normal",1,0)
y_pred2 = predict(classifier2, newdata = dataset2)
y_pred2<- ifelse(y_pred2=="normal",1,0)
y_pred3 = predict(classifier3, newdata = dataset2)
y_pred3<- ifelse(y_pred3=="normal",1,0)
y_pred4 = predict(classifier4, newdata = dataset2)
y_pred4<- ifelse(y_pred4=="normal",1,0)


# Making the Confusion Matrix
#Confusion Matrix
CFL<-table(y_pred1,dataset2$class)
CFR<-table(y_pred2,dataset2$class)
CFS<-table(y_pred3,dataset2$class)
CFP<-table(y_pred4,dataset2$class)

#Accuracy
AccL<-Acc(CFL)
AccL

AccR<-Acc(CFR)
AccR

AccS<-Acc(CFS)
AccS

AccP<-Acc(CFP)
AccP

#building the auc curve
#model1
pr_1<- prediction(y_pred1,dataset2$class)
prf<-performance(pr_1,measure="tpr",x.measure="fpr")
auc<- performance(pr_1,measure="auc")
auc3<-auc@y.values[[1]]
auc3
plot(prf)

#model 2
pr_2<- prediction(y_pred2,dataset2$class)
prf<-performance(pr_2,measure="tpr",x.measure="fpr")
auc<- performance(pr_2,measure="auc")
auc4<-auc@y.values[[1]]
auc4
plot(prf)

#model 3
pr_3<- prediction(y_pred3,dataset2$class)
prf<-performance(pr_3,measure="tpr",x.measure="fpr")
auc<- performance(pr_3,measure="auc")
auc4<-auc@y.values[[1]]
auc4
plot(prf)

#model 4
pr_4<- prediction(y_pred4,dataset2$class)
prf<-performance(pr_4,measure="tpr",x.measure="fpr")
auc<- performance(pr_4,measure="auc")
auc5<-auc@y.values[[1]]
auc5
plot(prf)

#naive bayes

library(e1071)

classifier5<-naiveBayes(class~.,data = dataset)

#predicting the model

y_pred5<-predict(classifier5,newdata = dataset2[-40])
y_pred5<- ifelse(y_pred5=="normal",1,0)

#confusion matrix

cm<-table(y_pred5,dataset2[,40])

cm
#accuracy of model
acc(cm)


# getting auc for naive bayes
pr_5<- prediction(y_pred5,dataset2$class)
prf<-performance(pr_5,measure="tpr",x.measure="fpr")
auc<- performance(pr_5,measure="auc")
auc6<-auc@y.values[[1]]
auc6
plot(prf)

#knn model

library(class)
y_pred6<-knn(train = dataset[,-40],test = dataset2[,-40],cl=dataset$class,k=200,prob=TRUE)

y_pred6<- ifelse(y_pred6=="normal",1,0)
#confusion matrix

cm<-table(y_pred6,dataset2[,40])

cm
#accuracy of model
acc(cm)

#getting auc for knn

pr_6<- prediction(y_pred6,dataset2$class)
prf<-performance(pr_6,measure="tpr",x.measure="fpr")
auc<- performance(pr_6,measure="auc")
auc7<-auc@y.values[[1]]
auc7
plot(prf)

getwd()
