getwd()

#put na.strings so dat blank shouls be treated as na
cr<-read.csv("R_Module_Day_7.2_Credit_Risk_Train_data.csv",na.strings=c(""," ","NA"))
#case1<-read.csv("R_Module_Day_7.2_Credit_Risk_Train_data.csv",na.strings=c(""," ","NA"))
View(cr)

#levels(cr$LoanAmount)
#class(cr$Self_Employed)

#EDA(explanatory data analysis)
summary(cr)
#is.na(cr)
#sum(is.na(cr))
#dim(cr)
colSums(is.na(cr))

#mode
getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
#getmode(cr$Credit_History)

#imput median in continuous variable
cr$Loan_Amount_Term[is.na(cr$Loan_Amount_Term)]<-median(cr$Loan_Amount_Term,na.rm=T)

cr$LoanAmount[is.na(cr$LoanAmount)]<-median(cr$LoanAmount,na.rm=T)

cr$Credit_History[is.na(cr$Credit_History)]<-getmode(cr$Credit_History)
cr$Gender[is.na(cr$Gender)]<-getmode(cr$Gender)
cr$Married[is.na(cr$Married)]<-getmode(cr$Married)

#levels(cr$Self_Employed)
cr$Self_Employed[is.na(cr$Self_Employed)]<-getmode(cr$Self_Employed)
cr$Dependents[is.na(cr$Dependents)]<-getmode(cr$Dependents)


#remove unique identifier
cr$Loan_ID<-NULL

summary(cr)

#converting variables into factors
cr$Credit_History<-as.factor(cr$Credit_History)

#svm model
library(e1071)

attach(cr)
classifier<- svm(formula=Loan_Status~.,data=cr,kernel='radial')
#classifier<- svm(formula=Loan_Status~.,data=cr,type='C-classification',kernel='linear')


#read validate file
cr_v<-read.csv("R_Module_Day_8.2_Credit_Risk_Validate_data.csv",na.strings=c(""," ","NA"))
summary(cr_v)
View(cr_v)

#levels(cr$LoanAmount)
#class(cr$Self_Employed)

#EDA(explanatory data analysis)
summary(cr_v)
colSums(is.na(cr_v))
cr_v$Credit_History<-as.factor(cr_v$Credit_History)


cr_v$Loan_Amount_Term[is.na(cr_v$Loan_Amount_Term)]<-median(cr_v$Loan_Amount_Term,na.rm=T)
cr_v$LoanAmount[is.na(cr_v$LoanAmount)]<-median(cr_v$LoanAmount,na.rm=T)

cr_v$Credit_History[is.na(cr_v$Credit_History)]<-getmode(cr_v$Credit_History)
cr_v$Gender[is.na(cr_v$Gender)]<-getmode(cr_v$Gender)
cr_v$Married[is.na(cr_v$Married)]<-getmode(cr_v$Married)

#levels(cr$Self_Employed)
cr_v$Self_Employed[is.na(cr_v$Self_Employed)]<-getmode(cr_v$Self_Employed)
cr_v$Dependents[is.na(cr_v$Dependents)]<-getmode(cr_v$Dependents)


#remove unique identifier
cr_v$Loan_ID<-NULL

summary(cr_v)
colSums(is.na(cr_v))

#for svm define the indep. variables nd depen. variable seperately
cr_v_ind<-cr_v[,-12]
cr_v_dep<-cr_v$Loan_Status

#prediction the validate set result
y_pred=predict(classifier,cr_v[,-12])

#support vector machine




#making the confusion matrix
cm=table(y_pred,cr_v_dep)

Acc=function(cf1){
  tp=cf1[2,2]
  fp=cf1[2,1]
  Acc=tp/(tp+fp)
  Acc
}
Acc(cm)
