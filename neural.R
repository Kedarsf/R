getwd()

neurl<-read.csv('Churn_Modelling.csv')

colSums(is.na(neurl))

neurl<-neurl[4:14]

neurl$Geography<-as.numeric(factor(neurl$Geography))
neurl$Gender<-as.numeric(factor(neurl$Gender))

scale(neurl[,1:10])

library(neuralnet)
View(neurl)
n<-names(neurl)
f<-as.formula(paste("Exited~",paste(n[!n%in%"Exited"],collapse = "+")))
nn<-neuralnet(f,data = neurl,linear.output = FALSE,hidden = 2)
plot(nn)

#prediction

