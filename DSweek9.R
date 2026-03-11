data(mtcars)
head(mtcars)
str(mtcars)
model <- lm (mpg ~ hp + wt + cyl, data = mtcars)
summary(model)
data(mtcars)
data.train <- mtcars[1:22,]
data.test <- mtcars[23:32,]
relation <- lm(mpg ~ hp + wt+cyl, data = data.train)
summary(relation)
a <- data.frame(hp = data.test$hp, wt = data.test$wt, cyl = data.test$cyl)
result <- predict(relation, a)
print(round(result, digits = 2))
mape <- mean(abs((data.test$mpg - result)/ data.test$mpg )*100)
paste("The error - MAPE is: ", round(mape,digit=2),"%")
actuals_preds <- data.frame(cbind(actuals=data.test$mpg, predicted=result))
View(actuals_preds )
correlation_accuracy <- cor(actuals_preds)
mape <- mean(abs(actuals_preds$actuals - actuals_preds$predicted)/actuals_preds$actuals)*100
paste("The error - MAPE is: ", round(mape,digit=2),"%")
actuals_preds <- data.frame(cbind(actuals=data.test$mpg, predicted=result))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals)) / actuals_preds$actuals) * 100
paste("The error - MAPE is: ", round(mape, digit = 2), "%")

