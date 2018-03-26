
#data preprocessing

dataset= read.csv('Position_Salaries.csv')

View(dataset)

colSums(is.na(dataset))

dataset = dataset[2:3]

# fitting the SVR model 
library(e1071)

model = svm(formula = Salary ~ . ,
            data = dataset,
            type = 'eps-regression')

# predicting the salary 

y_pred = predict(model, data.frame(Level = 6.5))
# visualizing the linear model

library(ggplot2)

ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = dataset$Level , y = predict(model , newdata = dataset)),
            color = 'blue') +
  ggtitle('salary vs level') +
  xlab('level') +
  ylab('Salary')


ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = dataset$Level , y = predict(poly_reg , newdata = dataset)),
            color = 'blue') +
  ggtitle('salary vs level') +
  xlab('level') +
  ylab('Salary')
