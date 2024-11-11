data <- read.csv("cleaned_data.csv")

library(DataExplorer)
library(ggplot2)
plot_missing(data)
data
# Assuming 'data' is a data frame and contains the columns you're interested in
# Selecting specific columns
selected_columns <- data %>%
  select(name, height, ends_with("free"))

# View the selected columns
print(selected_columns)

View(data)
str(data$credit_class)  # Display the structure and type of credit_class

#Question1
# Convert credit_class to a binary factor if it isn’t already
data$credit_class <- as.factor(data$class)

# Fit the logistic regression model
model <- glm(credit_class ~ credit_amount, data = data, family = binomial)

# Summary of the model to view coefficients
summary(model)

# Plotting predicted probabilities of a good or bad credit class across credit amounts
data$predicted_prob <- predict(model, type = "response")

# Visualization
ggplot(data, aes(x = credit_amount, y = predicted_prob)) +
  geom_point(aes(color = credit_class)) +
  labs(title = "Predicted Probability of Credit Class based on Credit Amount",
       x = "Credit Amount",
       y = "Predicted Probability of Good Credit Class") +
  theme_minimal()

#----------------------------------------------------------
# Convert credit_class to a binary factor (0 for good, 1 for bad)
data$credit_class_binary <- ifelse(data$class == "bad", 1, 0)

# Fit logistic regression model
model <- glm(credit_class_binary ~ credit_amount, data = data, family = binomial)

# Generate a range of credit amounts to predict probabilities
credit_amount_range <- data.frame(credit_amount = seq(min(data$credit_amount), max(data$credit_amount), length.out = 100))
credit_amount_range$predicted_prob <- predict(model, credit_amount_range, type = "response")

# Plot the results
library(ggplot2)
ggplot(data = credit_amount_range, aes(x = credit_amount, y = predicted_prob)) +
  geom_line(color = "blue") +
  geom_point(data = data, aes(x = credit_amount, y = credit_class_binary, color = class), alpha = 0.3) +
  labs(
    title = "Probability of Bad Credit Class by Credit Amount",
    x = "Credit Amount",
    y = "Predicted Probability of Bad Credit Class"
  ) +
  theme_minimal()


#box plot
ggplot(data, aes(x = class, y = credit_amount, fill = class)) +
  geom_boxplot() +
  labs(title = "Distribution of Credit Amount by Credit Class",
       x = "Credit Class",
       y = "Credit Amount") +
  theme_minimal()

ggplot(data, aes(x = class, y = credit_amount, fill = class)) +
  geom_boxplot() +
  labs(title = "Credit Amount Distribution by Credit Class", x = "Credit Class", y = "Credit Amount") +
  theme_minimal()


#scatter plot
ggplot(data, aes(x = credit_amount, y = class, color = class)) +
  geom_jitter(width = 0.2) +
  labs(
    x = "Credit Amount",
    y = "Credit Class",
    title = "Scatter Plot of Credit Amount by Credit Class"
  ) +
  theme_minimal()