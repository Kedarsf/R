library(cluster)
require(ks)
install.packages("fpc")
#install.packages("broom")
#install.packages("tidy")
#library(broom)
library(fpc)
library(cluster)
data("unicef",package = "ks")
colSums(is.na(unicef))
summary(unicef)
plot(unicef$`Under-5`,unicef$`Ave life exp`)
boxplot(unicef)
x<-scale(unicef)
boxplot(x)
#kmeans clustering
set.seed(100)
fit<-kmeans(x,6)
attributes(fit)
#finding value of k from elbow plot
wssplot<-function(data,nc,seed=1234){
  
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<- sum(kmeans(data,centers = i)$withinss)
  }
  
  plot(1:nc,wss,type = "b",xlab = "number of clusters",ylab = "within groups sum of squares")
}
wssplot(x,nc=10)

# kmeans with optimum cluster solution
fitkm<-kmeans(x,2)$cluster

#method 2 kmeans using fpc package
#fit<-kmeansruns(x,krange=6,runs=100)

fitasw<-kmeansruns(x,krange = 2:10,criterion = "asw",critout = FALSE,runs = 100)
fitasw$cluster
