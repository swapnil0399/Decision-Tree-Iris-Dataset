# Swapnil Agrawal | Assignment 2 | CS 4375.001

# load R packages
library(rpart)
library(rpart.plot)

# load data Sets
data(iris)
head(iris)

# Print summary of the data set
summary(iris)

# Clean data set
dupli <- duplicated(iris)
which(dupli)
clean.iris <- unique(iris[complete.cases(iris),])

# Create a default decision tree using rpart with Species as the predicting factor
tree_default <- rpart(Species ~ ., data = clean.iris)
tree_default

# Print the tree using rplot
rpart.plot(tree_default, extra = 2, under = TRUE, varlen=0, faclen=0)

# Predict the Species based on the tree
head(predict(tree_default, clean.iris))

# Store the predictions in pred
pred <- predict(tree_default, clean.iris, type="class")
head(pred)

# Creating confusion tables based on the predictions
confusion_table <- table(clean.iris$Species, pred)
confusion_table

# Accuracy function to calculate resubmission errror and accuracy
accuracy <- function(truth, prediction)
{ 
  tbl <- table(truth, prediction) 
  correct <- sum(diag(tbl))
  error <- sum(tbl) - correct
  accuracy <- sum(diag(tbl))/sum(tbl)
  cat("Accuracy:", accuracy * 100, "%\n")
  cat("Resubmission Error:", error, "\n")
}

# Calculating accuracy and error using default tree
num_nodes <- 3
generalization_error <- (sum(confusion_table) - sum(diag(confusion_table)) + num_nodes * 0.5)/sum(confusion_table)
cat("Generalization Error: ", generalization_error * 100, "\n")
accuracy(clean.iris$Species, pred)

# Now creating full tree
# Creating full tree from clean iris data
tree_full <- rpart(Species ~., data=clean.iris, control=rpart.control(minsplit=2, cp=0))
# Plotting the tree
rpart.plot(tree_full, extra = 2, under = TRUE,  varlen=0, faclen=0)
head(predict(tree_full, clean.iris))

# Store the predictions in pred_full
pred_full <- predict(tree_full, clean.iris, type="class")
head(pred_full)

# Creating confusion table for the full tree
confusion_table_full <- table(clean.iris$Species, pred_full)
confusion_table_full

# Calculating error and accuracy for full tree
num_nodes <- 9
generalization_error <- (sum(confusion_table) - sum(diag(confusion_table)) + num_nodes * 0.5)/sum(confusion_table)
cat("Generalization Error: ", generalization_error * 100, "\n")
accuracy(clean.iris$Species, pred_full)

# Now creating a new flower whose species is versicolor
my_flower <- data.frame(Sepal.Length = 6.1, Sepal.Width = 2.6, Petal.Length = 4.0, Petal.Width = 1.2, Species = NA)
my_flower

#Make a prediction using the default tree and full tree for my flower
predict(tree_default , my_flower, type = "class")
predict(tree_full , my_flower, type = "class")

# Now predicting using holdout sample method
# Generating 2/3 data for training set
n_train <- as.integer(nrow(clean.iris)*.66)
n_train

# Random sampling for 66% values of iris dataset
train_id <- sample(1:nrow(clean.iris), n_train)
head(train_id)

train <- clean.iris[train_id,]   # training dataset
test <- clean.iris[-train_id, colnames(clean.iris) != "Species"] # testing dataset includes all columns except type 
test_type <- clean.iris[-train_id, "Species"]  #actual type for each flower in the test set
tree <- rpart(Species ~., data=train,control=rpart.control(minsplit=2)) # create a tree based on traing set

# Plotting the newly genrated tree
rpart.plot(tree, extra = 2, under = TRUE,  varlen=0, faclen=0)

# Calculating error and accuracy for train using training data and testing data
num_nodes <- 5
generalization_error <- (sum(confusion_table) - sum(diag(confusion_table)) + num_nodes * 0.5)/sum(confusion_table)
cat("Generalization Error: ", generalization_error * 100, "\n")
accuracy(train$Species, predict(tree, train, type="class"))
num_nodes <- 3
generalization_error <- (sum(confusion_table) - sum(diag(confusion_table)) + num_nodes * 0.5)/sum(confusion_table)
generalization_error * 100
accuracy(test_type, predict(tree, test, type="class"))



