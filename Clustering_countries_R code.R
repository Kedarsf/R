install.packages("ks")
install.packages("fpc")
install.packages("broom")
install.packages("tidy")
library(broom)
require(fpc)
library(cluster)
data("unicef",package = "ks")
colSums(is.na(unicef))
head(unicef)
summary(unicef)
plot(unicef$`Under-5`,unicef$`Ave life exp`)
boxplot(unicef)
x<-scale(unicef)
boxplot(x)
set.seed(2018)
fit<-kmeansruns(x,krange = 6,runs = 100)
summary(fit)
head(fit$cluster,3)
tidy(fit)
wssplot<-function(data,nc,seed=1234){
  
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<- sum(kmeansruns(data,centers = i)$withinss)
  }
  plot(1:nc,wss,type = "b",xlab = "number os clusters",ylab = "within groups sum of squares")
}
wssplot(x,nc=10)

fitasw<-kmeansruns(x,krange = 2:10,criterion = "asw",critout = FALSE,runs = 100)
clusplot(x,fitasw$cluster,color = TRUE,shade = TRUE,labels = 2,lines = 0)
round(fitasw$centers,3)
comview<-cbind(fitasw$cluster,unicef)
colnames(comview)<-c("cluster","median_under_5","median_life_exp")
tab1<-aggregate(data=comview,median_under_5~cluster,mean)
tab2<-aggregate(data = comview,median_life_exp~cluster,mean)
med<-cbind(tab1,tab2)
med<-med[,-3]
med

country<-rownames(x)
final_cluster<-lapply(1:2,function (cluster) country [fitasw$cluster == cluster])
library(car)
new<-cbind(fitasw$cluster,unicef)
new$`fitasw$cluster`<-recode(new$`fitasw$cluster`,"1=0")
new$`fitasw$cluster`<-recode(new$`fitasw$cluster`,"2=1")

predd<-glm(new$`fitasw$cluster` ~new$`Under-5`+new$`Ave life exp`,data = new,family = binomial(link = 'logit'))
