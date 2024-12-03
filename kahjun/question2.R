library(dplyr)
library(ggplot2)
library(gridExtra)

# Load the cleaned CSV file
data <- read.csv("cleaned_data.csv", stringsAsFactors = TRUE)

# # str(data)

# # Descriptive Statistics for property_magnitude, savings_status, credit_amount, and credit_class
# table(data$property_magnitude, data$class) # Frequency table for property_magnitude
# table(data$savings_status,data$class)      # Frequency table for savings_status
# table(data$credit_amount,data$class)       # Frequency table for credit_amount (this is numeric, so it might be better as a summary)
# summary(data$credit_amount)                # Summary statistics for credit_amount

# # Bivariate Analysis: 
# # Visualizing relationships between property_magnitude, savings_status, credit_amount, and class
# plot1 <- ggplot(data, aes(x = property_magnitude, fill = class)) +
#   geom_bar(position = "dodge") +
#   labs(title = "Property Magnitude vs Credit Class")
# 
# plot2 <- ggplot(data, aes(x = savings_status, fill = class)) +
#   geom_bar(position = "dodge") +
#   labs(title = "Savings Status vs Credit Class")
# 
# outlier_counts <- data %>%
#   group_by(class) %>%
#   summarise(
#     Q1 = quantile(credit_amount, 0.25, na.rm = TRUE),
#     Q3 = quantile(credit_amount, 0.75, na.rm = TRUE),
#     IQR = Q3 - Q1,
#     lower_threshold = Q1 - 1.5 * IQR,
#     upper_threshold = Q3 + 1.5 * IQR,
#     outlier_count = sum(credit_amount < lower_threshold | credit_amount > upper_threshold, na.rm = TRUE)
#   )
# outlier_counts
# 
# plot3 <- ggplot(data, aes(x = class, y = credit_amount, fill = class)) +  # Boxplot uses class on x-axis
#   geom_boxplot() +
#   labs(title = "Credit Amount vs Credit Class")
# 
# # Arrange plots in a 1-row, 3-column layout
# grid.arrange(plot1,plot3 , plot2,  nrow = 3)


# Additional features
# Fit a logistic regression model with only property_magnitude
logistic_model_property <- glm(class ~ property_magnitude, data = data, family = "binomial")

# Fit a logistic regression model with only credit_amount
logistic_model_credit_amount <- glm(class ~ credit_amount, data = data, family = "binomial")

# Fit a logistic regression model with only savings_status
logistic_model_savings <- glm(class ~ savings_status, data = data, family = "binomial")

# Compare the AIC of all models
AIC(logistic_model_property, logistic_model_credit_amount, logistic_model_savings)

