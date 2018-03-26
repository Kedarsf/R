# decision tree regression


#data preprocessing

dataset= read.csv('Position_Salaries.csv')

View(dataset)

colSums(is.na(dataset))

dataset = dataset[2:3]

# fitting the Decision tree regression  model 
library(rpart)

model = rpart(formula = Salary ~ . , data = dataset , control = rpart.control(minsplit = 1))

# predicting the salary 

y_pred = predict(model, data.frame(Level = 6.5))
# visualizing the Decision tree regression model

library(ggplot2)

x_grid = seq(min(dataset$Level),max(dataset$Level),0.01)
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = 'red') +
  geom_line(aes(x = x_grid , y = predict(model , newdata = data.frame(Level = x_grid))),
            color = 'blue') +
  ggtitle('salary vs level') +
  xlab('level') +
  ylab('Salary')