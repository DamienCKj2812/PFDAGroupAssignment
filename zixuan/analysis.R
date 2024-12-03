data <- read.csv("cleaned_data.csv")

library(DataExplorer)
library(ggplot2)
plot_missing(data)
library(dplyr)
data

unique(data$foreign_worker)

freq_table <-table(data$employment)
print(freq_table)
selected_columns <- data %>%
  select(name, height, ends_with("free"))

#----------------------------------------------------------Question1--------------------------------------------------------
#----------------------------------------box plot------------------------------------------------------------------------
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

#--------------------------------------------------logistic regression------------------------------------------------
# Convert class to binary (0 = good, 1 = bad)
data$class <- ifelse(data$class == "bad", 1, 0)

# Fit logistic regression
logistic_model <- glm(class ~ credit_amount, data = data, family = binomial)

# Summary of the model
summary(logistic_model)

# Calculate odds ratio
exp(coef(logistic_model))

# Confidence intervals for the odds ratio
exp(confint(logistic_model))


# Create a subset with only credit_amount and predicted_prob
predicted_data <- data %>%
  select(credit_amount, predicted_prob)

# View the first few rows of the new data frame
head(predicted_data)



ggplot(data, aes(x = credit_amount, y = predicted_prob)) +
  geom_point(alpha = 0.5, size = 3) +  # Increase size of the points
  geom_smooth(method = "loess", color = "salmon") +
  labs(title = "Predicted Probability of Bad Credit by Credit Amount",
       x = "Credit Amount", y = "Probability of Bad Credit") +
  theme_minimal()

#------------------------------------------Question2--------------------------------------------------------------
# Bar chart for categorical employment duration
ggplot(data, aes(x = employment, fill = class)) +
  geom_bar(position = "dodge", stat="count") +
  labs(title = "Employment Duration Categories by Credit Class",
       x = "Employment Duration Category",
       y = "Count") +
  scale_fill_manual(values = c("good" = "skyblue", "bad" = "salmon")) +
  theme_minimal()

#--------------------------------------relationship between foreign worker and employment duration-----------------------------
library(dplyr)

summary_table <- data %>%
  group_by(foreign_worker, employment, class) %>%
  summarise(count = n(), .groups = "drop")

print(summary_table)

# Bar chart visualization
library(ggplot2)

ggplot(data, aes(x = employment, fill = class)) +
  geom_bar(position = "fill", stat = "count") +
  facet_wrap(~ foreign_worker) +
  labs(title = "Proportion of Credit Class by Employment Duration and Foreign Worker Status",
       x = "Employment Duration Category",
       y = "Proportion",
       fill = "Credit Class") +
  scale_fill_manual(values = c("good" = "skyblue", "bad" = "salmon")) +
  theme_minimal()

#-----------------------------------------chisquare----------------------------------------------
#Look for counts for employment duration and credit class
freq_table <- table(data$employment, data$class)
# Print the frequency table
print(freq_table)

employment_duration <- c("<1 year", ">=7 years", "1<=X<4 years", "4<=X<7 years", "unemployed")
bad_credit <- c(718, 400, 1148, 613, 121)  # Counts for bad credit class
good_credit <- c(409, 831, 1008, 578, 174)  # Counts for good credit class

# Create a contingency table
contingency_table <- data.frame(
  Employment_Duration = employment_duration,
  Good_Credit = good_credit,
  Bad_Credit = bad_credit
)

# View the contingency table
print(contingency_table)

# Convert to a matrix format suitable for the chi-square test
chi_table <- as.matrix(contingency_table[, -1])  # Exclude the first column (Employment_Duration)

# Perform the chi-square test
chi_test_result <- chisq.test(chi_table)

# View the test result
print(chi_test_result)






