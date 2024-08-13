rm(list = ls())

DF <- read_excel("P:/College/SEM - 6/LAB/DMPM_Lab/Titanic_DFset.xlsx")
View(DF)

DF$age[is.na(DF$age)] <- mean(DF$age, na.rm = TRUE)
DF$fare[is.na(DF$fare)] <- mean(DF$fare, na.rm = TRUE) 

plot(DF$age, DF$fare,
     xlab = "Age",
     ylab = "Fare",
     main = "Scatter Plot of Age vs Fare")

install.packages('caTools')
library(caTools)

split = sample.split(DF$fare, SplitRatio = 0.7)
trainingset = subset(DF, split == TRUE)
testset = subset(DF, split == FALSE)

lm.r = lm(fare ~ age, DF)
lm.r

summary(lm.r)

Pre = predict(lm.r, DF)
Pre

library(ggplot2)

ggplot() + geom_point(aes(x = trainingset$age, y = trainingset$fare), colour = 'red') +
  geom_line(aes(x = trainingset$age,	y = predict(lm.r, newdata = trainingset)), colour = 'blue') +
  ggtitle('Fare vs Age (Training set)') +
  xlab('Age') +
  ylab('Fare')

ggplot() + geom_point(aes(x = testset$age, y = testset$fare), colour = 'red') +
  geom_line(aes(x = testset$age,	y = predict(lm.r, newdata = testset)), colour = 'blue') +
  ggtitle('Fare vs Age (Test set)') +
  xlab('Age') +
  ylab('Fare')

DIFF = (DF$fare) - (Pre)
DIFF

#parameter's to evaluate linear model
MSE = mean((DIFF)^2) #mean square error
MSE

MAE = mean(abs(DIFF)) #mean absolute error
MAE

RMSE = sqrt(MSE) #root mean square error
RMSE

R_2 = 1 - (sum((DIFF)^2) / sum((DF$fare - mean(DF$fare))^2))
R_2

cat("MAE: ", MAE, "\n", "MSE: ", MSE, "\n", "RMSE: ", RMSE, "\n", "R-squared: ", R_2)
