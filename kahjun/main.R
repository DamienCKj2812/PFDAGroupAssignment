# Load necessary libraries
# install.packages("stringi")
# install.packages("caret")
# install.packages("dplyr")
# install.packages("glmnet")
# install.packages("randomForest")
# install.packages("class")
library(tidyverse)
library(caret)
library(glmnet)
library(randomForest)
library(class)

# Load the cleaned CSV file
data <- read.csv("cleaned_data.csv")

# Convert class variable to factor ---------------------------------------------
data$class <- as.factor(data$class)
data$savings_status <- as.factor(data$savings_status)
data$property_magnitude <- as.factor(data$property_magnitude)  

print("Levels for class:")
print(levels(data$class))

print("Levels for savings_status:")
print(levels(data$savings_status))

print("Levels for property_magnitude:")
print(levels(data$property_magnitude))

print(levels(as.factor(data$personal_status)))

# ------------------------------------------------------------------------------


# Question 1: How does savings status correlate with credit class??
# Recode savings_status to add a new column savings_condition based on the updated conditions
data$savings_condition <- with(data, ifelse(savings_status %in% c("500<=X<10000", ">=1000"), "good",
                                            ifelse(savings_status %in% c("<100", "100<=X<500", "no known savings"), "bad", NA)))

# Convert savings_condition to factor
data$savings_condition <- as.factor(data$savings_condition)

# Ensure class is also a factor
data$class <- as.factor(data$class)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$class, p = .8, list = FALSE, times = 1)
dataTrain <- data[trainIndex, ]
dataTest <- data[-trainIndex, ]

# Fit the Random Forest model with savings_status, savings_condition, and other predictors if necessary
random_forest_model <- randomForest(class ~ savings_status + savings_condition, data = dataTrain, importance = TRUE)

# Print model summary
print(random_forest_model)

# Check variable importance to see if savings_condition has a notable impact
importance(random_forest_model)
varImpPlot(random_forest_model)

# Make predictions on the test set
test_predictions <- predict(random_forest_model, newdata = dataTest)

# Create a confusion matrix to evaluate performance
confusion_matrix <- table(Predicted = test_predictions, Actual = dataTest$class)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Test Accuracy:", round(accuracy * 100, 2), "%"))



train_predictions <- predict(random_forest_model, newdata = dataTrain)

# Create a confusion matrix for the training set
train_confusion_matrix <- table(Predicted = train_predictions, Actual = dataTrain$class)
print("Training Confusion Matrix:")
print(train_confusion_matrix)

# Calculate training accuracy
train_accuracy <- sum(diag(train_confusion_matrix)) / sum(train_confusion_matrix)
print(paste("Training Accuracy:", round(train_accuracy * 100, 2), "%"))







# # Question 3: How does the interaction between savings status and property magnitude impact credit classification?
# # Set seed for reproducibility
# set.seed(123)
# 
# # Split the data into training and testing sets
# trainIndex <- createDataPartition(data$class, p = .8, list = FALSE, times = 1)
# dataTrain <- data[trainIndex, ]
# dataTest  <- data[-trainIndex, ]
# 
# # Fit the Logistic Regression model
# logistic_model <- glm(class ~ savings_status + property_magnitude,
#                       data = dataTrain,
#                       family = binomial)
# 
# # Summarize the model
# summary(logistic_model)
# 
# # Make predictions on the training set
# train_predictions_prob <- predict(logistic_model, newdata = dataTrain, type = "response")
# train_predictions <- ifelse(train_predictions_prob > 0.5, "good", "bad")
# train_predictions <- as.factor(train_predictions)
# 
# # Create a confusion matrix to evaluate the model on training data
# train_confusion_matrix <- table(Predicted = train_predictions, Actual = dataTrain$class)
# 
# # Print the confusion matrix for training data
# print("Training Confusion Matrix:")
# print(train_confusion_matrix)
# 
# # Calculate training accuracy
# train_accuracy <- sum(diag(train_confusion_matrix)) / sum(train_confusion_matrix)
# print(paste("Training Accuracy:", round(train_accuracy * 100, 2), "%"))
# 
# # Make predictions on the test set
# predictions_prob <- predict(logistic_model, newdata = dataTest, type = "response")
# predictions <- ifelse(predictions_prob > 0.5, "good", "bad")
# predictions <- as.factor(predictions)
# 
# # Create a confusion matrix to evaluate the model on test data
# confusion_matrix <- table(Predicted = predictions, Actual = dataTest$class)
# 
# # Print the confusion matrix for test data
# print("Test Confusion Matrix:")
# print(confusion_matrix)
# 
# # Calculate test accuracy
# accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
# print(paste("Test Accuracy:", round(accuracy * 100, 2), "%"))
# 
# # Calculate additional performance metrics for test data
# confusion_stats <- confusionMatrix(predictions, dataTest$class)
# print(confusion_stats)

