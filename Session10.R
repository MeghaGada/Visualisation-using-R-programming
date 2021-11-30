library(ISLR)
dim(Auto)
Auto[1:4,]
Auto = na.omit(Auto)
names(Auto)
a = Auto
plot(a$cylinder,a$mpg)
attach(a)
cylinders = as.factor(cylinders)
plot(cylinders,mpg)
plot(cylinders,mpg,col="green",varwidth = T,xlab = "cylinders",ylab = "MPG")
plot(cylinders,mpg,col="green",varwidth = T)

hist(mpg,col = 3,breaks = 15)
pairs(a)
pairs(~mpg+displacement+horsepower+weight+acceleration,a)
plot(horsepower,mpg)
summary(a)
head(a)
df = a
head(df)
library(caret)


validation_index <- createDataPartition(horsepower, p=0.80, list=FALSE)
validation <- df[-validation_index,]
df <- df[validation_index,]
control <- trainControl(method="cv", number=10)
metric <- "none"


#Linear Regression
set.seed(7)
fit.lm <- train(horsepower~., data=df, method="lm", metric=metric, trControl=control)
print(fit.lm)
predictions <- predict(fit.lm, validation)
r21 = R2(validation$horsepower, predictions, form = "traditional")
print(r21)

#SVM regression
library(e1071)
model_reg = svm(horsepower~., data=df)
print(model_reg)
pred = predict(model_reg, validation)
print(pred)
r2 = R2(validation$horsepower, pred, form = "traditional")
print(r2)

#Random forest
fit.rf <- train(horsepower~., data=df, method="rf", metric=metric, trControl=control)
predictions1 <- predict(fit.rf, validation)
r22 = R2(validation$horsepower, predictions1, form = "traditional")
print(r22)

#Decision Tree Regression
library(rpart)
fit.rpart <- train(horsepower~., data=df, method="rpart", metric=metric, trControl=control)
predictions2 <- predict(fit.rpart, validation)
r23 = R2(validation$horsepower, predictions2, form = "traditional")
print(r23)

cat("Accuracy_SVM",r2,"\n","Accuracy_linear_regression",r21,"\n","Accuracy_random_forest_regression",r22,"\n","Accuracy_decision_regression",r23)

