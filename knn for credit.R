getwd()

cr<-read.csv("C:/Users/Admin/Documents/R_Module_Day_7.2_Credit_Risk_Train_data.csv",na.strings = c(" ","","NA"))

View(cr)

summary(cr)

colSums(is.na(cr))

mode<- function(v){
  uniqv<- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# imputation 

cr$LoanAmount[is.na(cr$LoanAmount)]<-mean(cr$LoanAmount,na.rm = T)

cr$Loan_Amount_Term[is.na(cr$Loan_Amount_Term)]<-median(cr$Loan_Amount_Term,na.rm = T)
cr$Credit_History[is.na(cr$Credit_History)]<-mode(cr$Credit_History)

summary(cr)

cr$Self_Employed[is.na(cr$Self_Employed)]<-mode(cr$Self_Employed)

cr$Dependents[is.na(cr$Dependents)]<-mode(cr$Dependents)

cr$Gender[is.na(cr$Gender)]<-mode(cr$Gender)

cr$Married[is.na(cr$Married)]<-mode(cr$Married)

# removing the unique variables

cr$Loan_ID<-NULL

# converting catagorical variable into factors


cr$Credit_History<- as.numeric(cr$Credit_History)
class(cr$Credit_History)

cr$Gender<-as.numeric(cr$Gender)

cr$Married<-as.numeric(cr$Married)

cr$Self_Employed<-as.numeric(cr$Self_Employed)

cr$Dependents<-as.numeric(cr$Dependents)

cr$Education<-as.numeric(cr$Education)

cr$Property_Area<-as.numeric(cr$Property_Area)


# model fitting


model1<-glm(Loan_Status~.,family = binomial(link = 'logit'), data=cr)
summary(model1)

model2<-glm(Loan_Status~Married+Credit_History+Property_Area,family = binomial(link = 'logit'), data=cr)
summary(model2)
# we are validating the model

cr.v<-read.csv("C:/Users/Admin/Documents/R_Module_Day_8.2_Credit_Risk_Validate_data.csv",na.strings = c(" ","","NA"))

# checking for the missing values

colSums(is.na(cr.v))

# imputation of the data

cr.v$Loan_Amount_Term[is.na(cr.v$Loan_Amount_Term)]<-median(cr.v$Loan_Amount_Term,na.rm = T)
cr.v$Credit_History[is.na(cr.v$Credit_History)]<-mode(cr.v$Credit_History)

cr.v$LoanAmount[is.na(cr.v$LoanAmount)]<-median(cr.v$LoanAmount,na.rm = T)

cr.v$Self_Employed[is.na(cr.v$Self_Employed)]<-mode(cr.v$Self_Employed)

cr.v$Dependents[is.na(cr.v$Dependents)]<-mode(cr.v$Dependents)

cr.v$Gender[is.na(cr.v$Gender)]<-mode(cr.v$Gender)

summary(cr.v$Loan_Status)

summary(cr.v$LoanAmount)

cr.v$Loan_Status<-ifelse(cr.v$Loan_Status=="Y",1,0)
cr.v<-cr.v[,-1]
cr.v$Credit_History<- as.numeric(cr.v$Credit_History)
class(cr$Credit_History)

cr.v$Gender<-as.numeric(cr.v$Gender)

cr.v$Married<-as.numeric(cr.v$Married)

cr.v$Self_Employed<-as.numeric(cr.v$Self_Employed)

cr.v$Dependents<-as.numeric(cr.v$Dependents)

cr.v$Education<-as.numeric(cr.v$Education)

cr.v$Property_Area<-as.numeric(cr.v$Property_Area)

View(cr.v)
# validating our model using validation set

fitted.results1 <- predict(model2,newdata=cr.v,type='response')
fitted.results1 <- ifelse(fitted.results1 >=0.5,1,0)

summary(fitted.results1)

#Confusion matrix
cf1<-table(fitted.results1,cr.v$Loan_Status)
cf1
#function for accuracy
acc<-function(cf1){
  TP=cf1[2,2]
  FP=cf1[2,1]
  acc=TP/(TP+FP)
  acc
}

acc(cf1)

#test data

cr.t<- read.csv("C:/Users/Admin/Documents/R_Module_Day_8.1_Credit_Risk_Test_data.csv",na.strings = c(" ","","NA"))
summary(cr.t)

View(cr.t)


colSums(is.na(cr.t))

cr.t$Credit_History<-factor(cr.t$Credit_History)
cr.t$Loan_ID=NULL

cr.t$Loan_Amount_Term[is.na(cr.t$Loan_Amount_Term)]<-median(cr.t$Loan_Amount_Term,na.rm=T)
cr.t$LoanAmount[is.na(cr.t$LoanAmount)]<-median(cr.t$LoanAmount,na.rm=T)

cr.t$Gender[is.na(cr.t$Gender)]<-mode(cr.t$Gender)
cr.t$Dependents[is.na(cr.t$Dependents)]<-mode(cr.t$Dependents)

cr.t$Self_Employed[is.na(cr.t$Self_Employed)]<-mode(cr.t$Self_Employed)
cr.t$Credit_History[is.na(cr.t$Credit_History)]<-mode(cr.t$Credit_History)

pred1<-predict(model1,cr.t,'response')
pred1<-ifelse(pred1>=0.5,"Y","N")
cr.t$loan_stat<-pred1
cr.t<-data.frame(cr.t,pred1)

View(cr.t)

library(ROCR)
#plotting auc curve

pr<- prediction(fitted.results1,cr.v$Loan_Status)
prf<-performance(pr,measure="tpr",x.measure="fpr")
auc<- performance(pr,measure="auc")
auc1<-auc@y.values[[1]]
auc1
plot(prf)


#knn for credit risk
View(cr)
library(class)
cr$Loan_Status<-ifelse(cr$Loan_Status=="Y",1,0)
cr.v[,12]<-as.factor(cr.v[,12])
ypred6<-knn(train = cr[,-12],test = cr.v[,-12],cl=cr$Loan_Status,k=100)

ypred6<- ifelse(ypred6=="Y",1,0)
#confusion matrix

cm<-table(ypred6,cr.v[,12])

cm
#accuracy of model
acc(cm)

#getting auc for knn

pr_6<- prediction(ypred6,cr.v$Loan_Status)
prf<-performance(pr_6,measure="tpr",x.measure="fpr")
auc<- performance(pr_6,measure="auc")
auc7<-auc@y.values[[1]]
auc7
plot(prf)
