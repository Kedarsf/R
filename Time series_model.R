#Time series
#Reading tractor sales data
data<-read.csv(url("http://www.ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-Sales.csv"))
View(data)
#Step 1.Data into time series data
data = ts(data[,2],start = c(2003,1),frequency = 12)
#plot the time series data
plot(data, xlab='Years', ylab = 'Tractor Sales')
#Clearly the above chart has an upward trend for tractors sales 
#and there is also a seasonal component 

#Step2. Stationary series:Difference data to make data stationary on mean (remove trend)
plot(diff(data),ylab='Differenced Tractor Sales')

#Step3. log transform data to make data stationary on variance
plot(log10(data),ylab='Log (Tractor Sales)')

#Difference log transform data to make data stationary on both mean and variance
plot(diff(log10(data)),ylab='Differenced Log (Tractor Sales)')

#this series looks stationary on both mean and variance

#Step5. Plot ACF and PACF to identify potential AR and MA model
#Now, let us create autocorrelation factor (ACF) and partial autocorrelation factor (PACF) plots to identify patterns in the above data 
#which is stationary on both mean and variance. 
#The idea is to identify presence of AR and MA components in the residuals. 
#The following is the R code to produce ACF and PACF plots.
par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main='ACF Tractor Sales')
pacf(ts(diff(log10(data))),main='PACF Tractor Sales')

#Step6. Identification of best fit ARIMA model
require(forecast)
ARIMAfit = auto.arima(log10(data), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

#Step7. Forecast sales using the best fit ARIMA model
#The next step is to predict tractor sales for next 3 years i.e. for 2015, 2016, and 2017 through the above model. 
par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 36)
pred
#plot(data,type='l',xlim=c(2004,2018),ylim=c(1,1600),xlab = 'Year',ylab = 'Tractor Sales')
#lines(10^(pred$pred),col='blue')
#lines(10^(pred$pred+2*pred$se),col='orange')
#lines(10^(pred$pred-2*pred$se),col='orange')

