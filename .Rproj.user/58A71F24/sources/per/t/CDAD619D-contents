library(caret)
library(rpart)
library(rpart.plot)
data(iris)

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# Fit Naive Bayes Model
model <- train(Species~., data=iris, trControl=train_control, method="rpart")
# Summarise Results
print(model)

rpart.plot(model$finalModel, extra = 2, under = TRUE, varlen = 0, faclen = 0)