library(caTools)

data <- read.csv("cleaned_data.csv", stringsAsFactors = TRUE)

# str(data)

# Question 3:How does the interaction between savings status and property magnitude impact credit classification?

set.seed(123)  # For reproducibility

# Split the data into training and test sets using sample.split
split <- sample.split(data, SplitRatio = 0.8) 

# Create the training set (TRUE for training set)
train_set <- subset(data, split == TRUE)

# Create the test set (FALSE for test set)
test_set <- subset(data, split == FALSE)


dim(train_set)
dim(test_set)


# Build the multiple logistic regression model
logistic_model <- glm(class ~ property_magnitude + savings_status, data = train_set, family = binomial)

# View the summary of the model to interpret the results
summary(logistic_model)

# Training accuracy
# Predict probabilities on the training set
train_predictions <- predict(logistic_model, newdata = train_set, type = "response")
train_predicted_class <- ifelse(train_predictions > 0.5, "good", "bad")

# Create a confusion matrix for the training set
train_confusion_matrix <- table(Predicted = train_predicted_class, Actual = train_set$class)

# Calculate training accuracy
train_accuracy <- sum(diag(train_confusion_matrix)) / sum(train_confusion_matrix)
train_accuracy

# Test accuracy
# Predict probabilities on the test set
test_predictions <- predict(logistic_model, newdata = test_set, type = "response")
test_predicted_class <- ifelse(test_predictions > 0.5, "good", "bad")

# Create a confusion matrix for the test set
test_confusion_matrix <- table(Predicted = test_predicted_class, Actual = test_set$class)

# Calculate test accuracy
test_accuracy <- sum(diag(test_confusion_matrix)) / sum(test_confusion_matrix)
test_accuracy

