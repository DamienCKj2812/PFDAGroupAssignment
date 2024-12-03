# Load necessary libraries
# install.packages("VIM")
# install.packages("caret")
# install.packages("RANN")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("mice")
# install.packages("magrittr")
library(VIM)
library(caret)
library(RANN)
library(tidyr)
library(dplyr)
library(mice)
library(magrittr)


data <- read.csv("./data_cleaning/5. credit_risk_classification.csv")

# Imputation functions -------------------------------------------------------------

# Function to remove rows with high percentages of NA values
remove_high_na_rows <- function(data, threshold = 70) {
  na_percentage <- rowMeans(is.na(data)) * 100
  cleaned_data <- data[na_percentage <= threshold, ]
  return(cleaned_data)
}

mean_median_mode_imputation <- function(data, column_name, method = c("mean", "median", "mode")) {
  method <- match.arg(method)
  column <- data[[column_name]]
  
  if (method == "mode") {
    # Mode calculation for categorical data
    if (is.character(column) || is.factor(column)) {
      mode_value <- names(sort(table(column), decreasing = TRUE))[1]
      data[[column_name]][is.na(column)] <- mode_value
    } else {
      cat("Mode is not applicable for numerical data in this context.\n")
    }
  } else {
    # For mean and median, only apply to numeric data
    if (!is.numeric(column)) {
      stop("Mean and median imputations can only be applied to numeric columns.")
    }
    if (any(column < 0, na.rm = TRUE)) {
      cat("Warning: There are negative values in '", column_name, "'.\n", sep = "")
      return(data)
    }
    
    highest_value <- max(column, na.rm = TRUE)
    lowest_value <- min(column, na.rm = TRUE)
    cat("Highest value in '", column_name, "': ", highest_value, "\n", sep = "")
    cat("Lowest value in '", column_name, "': ", lowest_value, "\n", sep = "")
    
    if (any(is.na(column))) {
      imputed_value <- switch(method,
                              mean = mean(column, na.rm = TRUE),
                              median = median(column, na.rm = TRUE))
      data[[column_name]][is.na(column)] <- imputed_value
    } else {
      cat("No missing values in '", column_name, "'.\n", sep = "")
    }
  }
  
  return(data)
}


mice_imputation <- function(data, column_name, m = 5) {
  # Check if the specified column exists in the data
  if (!(column_name %in% names(data))) 
    stop(paste("Error: Column", column_name, "does not exist."))
  
  # Convert the specified column to a factor, which is necessary for categorical imputation
  data[[column_name]] <- as.factor(data[[column_name]])
  
  # Create a method list for imputation, specifying methods for each column
  imputation_methods <- make.method(data)
  
  # Set the imputation method for the target column to "polyreg" (polynomial regression)
  imputation_methods[column_name] <- "polyreg"
  
  # Perform the multiple imputation using the mice package
  # m specifies the number of multiple imputations to create
  imputed_data <- mice(data, method = imputation_methods, m = m)
  
  # Extract the completed dataset from the imputed data; 1 specifies the first imputed dataset
  completed_data <- complete(imputed_data, 1)
  
  # Return the dataset with imputed values
  return(completed_data)
}

replace_empty_with_na <- function(data, column_name) {
  if (!column_name %in% names(data)) stop(paste("Column", column_name, "does not exist."))
  data[[column_name]][data[[column_name]] == ""] <- NA
  return(data)
}

knn_imputation <- function(data, target_column, class_column, k_value = 10) {
  # Create a new data frame with the target column and the class column for imputation
  df <- data.frame(credit_hist = data[[target_column]], credit_class = data[[class_column]])
  
  # Convert the target column to a factor to ensure categorical handling in KNN
  df$credit_hist <- as.factor(df$credit_hist)
  
  # Perform K-Nearest Neighbors imputation on the target column
  # The variable to impute is specified, along with the number of neighbors (k_value)
  imputed_data <- kNN(df, variable = "credit_hist", k = k_value)
  
  # Replace the original target column in the data with the imputed values
  data[[target_column]] <- imputed_data$credit_hist
  
  # Return the modified data with imputed values
  return(data)
}

# Only for "age" category
age_hot_deck_imputation <- function(data, age_column, employment_column) {
  # Create a data frame containing the age column and employment years column
  df <- data.frame(age = data[[age_column]], years_of_employment = data[[employment_column]])
  
  # Define a function to categorize the years of employment into specific categories
  categorize_employment_years <- function(years) {
    if (is.na(years)) return(NA)  # Return NA for missing values
    else if (years == "unemployed") return("Unemployment")
    else if (years == "<1") return("0 to 1 year")
    else if (years == "1<=X<4") return("1 to 4 years")
    else if (years == "4<=X<7") return("4 to 7 years")
    else if (years == ">=7") return("More than 7 years")
  }
  
  # Apply the categorization function to create a new column for employment categories
  df$employment_category <- sapply(df$years_of_employment, categorize_employment_years)
  
  # Loop through each index in the data frame where age is missing
  for (index in which(is.na(df$age))) {
    # Select donors (rows with known years of employment)
    donors <- df[!is.na(df$years_of_employment), ]
    
    # If no donors are available, skip to the next iteration
    if (nrow(donors) == 0) next
    
    # Randomly sample a donor row from those available
    donor_row <- donors[sample(nrow(donors), 1), ]
    
    # Impute age based on the employment category of the donor
    if (donor_row$years_of_employment == "unemployed") {
      # Impute using the mean age of those who are unemployed
      df$age[index] <- floor(mean(data[[age_column]][data[[employment_column]] == "unemployed"], na.rm = TRUE))
    } else if (donor_row$years_of_employment %in% c("<1", "1<=X<4", "4<=X<7")) {
      # Impute using the mean age of those in the respective employment categories
      df$age[index] <- floor(mean(data[[age_column]][data[[employment_column]] == "<1"], na.rm = TRUE))
    } else if (donor_row$years_of_employment == ">=7") {
      df$age[index] <- floor(mean(data[[age_column]][data[[employment_column]] == ">=7"], na.rm = TRUE))
    }
  }
  
  # Update the original data frame with the imputed age values
  data[[age_column]] <- df$age
  
  # Return the modified data with imputed ages
  return(data)
}

# Pipeline to clean and impute data -------------------------------------------------


# Function to count NAs for each column
count_na <- function(data) {
  sapply(data, function(col) sum(is.na(col)))
}

# Count NAs before imputation
na_count_before <- count_na(data)
print("NA counts before imputation:")
print(na_count_before)

cleaned_data <- data %>%
  remove_high_na_rows() %>%
  replace_empty_with_na("checking_status") %>%
  mice_imputation("checking_status") %>%
  replace_empty_with_na("duration") %>%
  mean_median_mode_imputation("duration", method = "mean") %>%
  replace_empty_with_na("credit_history") %>%
  knn_imputation("credit_history", "class", k_value = 10) %>%
  replace_empty_with_na("purpose") %>%
  mice_imputation("purpose") %>%
  replace_empty_with_na("credit_amount") %>%
  mean_median_mode_imputation("credit_amount", method = "mean") %>%
  replace_empty_with_na("savings_status") %>%
  mice_imputation("savings_status") %>%
  replace_empty_with_na("employment") %>%
  mean_median_mode_imputation("employment", method = "mode") %>%
  replace_empty_with_na("installment_commitment") %>%
  knn_imputation("installment_commitment", "credit_history", k_value = 10) %>%
  replace_empty_with_na("personal_status") %>%
  mice_imputation("personal_status") %>%
  replace_empty_with_na("other_parties") %>%
  mice_imputation("other_parties") %>%
  replace_empty_with_na("residence_since") %>%
  mean_median_mode_imputation("residence_since", method = "mean") %>%
  replace_empty_with_na("property_magnitude") %>%
  mice_imputation("property_magnitude") %>%
  replace_empty_with_na("age")  %>%
  age_hot_deck_imputation("age", "employment") %>%
  replace_empty_with_na("other_payment_plans") %>%
  mice_imputation("other_payment_plans") %>%
  replace_empty_with_na("housing") %>%
  mice_imputation("housing") %>%
  replace_empty_with_na("existing_credits") %>%
  mean_median_mode_imputation("existing_credits", method = "median") %>%
  replace_empty_with_na("job") %>%
  mice_imputation("job") %>%
  replace_empty_with_na("num_dependents") %>%
  mice_imputation("num_dependents") %>%
  replace_empty_with_na("own_telephone") %>%
  mean_median_mode_imputation("own_telephone", method = "mode") %>%
  replace_empty_with_na("foreign_worker") %>%
  mean_median_mode_imputation("foreign_worker", method = "mode")

# Count NAs after imputation
na_count_after <- count_na(cleaned_data)
print("NA counts after imputation:")
print(na_count_after)

# Check the cleaned data
head(cleaned_data)
cleaned_data$c
# Write to new csv file
write.csv(cleaned_data, file = "./cleaned_data.csv", row.names = FALSE)

cleaned_data <- read.csv("C:\\Users\\user\\Desktop\\Programing for DA\\Assignment\\PFDAGroupAssignment\\cleaned_data.csv")

# Summarizing distribution of credit_class by checking_status
checking_status_summary=cleaned_data %>%
  group_by(checking_status,class) %>%
  summarise(count=n()) %>%
  mutate(percent=(count/sum(count)*100))

           
# Creating a bar chart to visualize the relationship
ggplot(checking_status_summary,aes(x=checking_status,y=percent,fill=class)) +
  geom_bar(stat="identity",position="dodge") +
  labs(title="Distribution of Credit Class by Checking Status",
       x="Checking Status",
       y="Percentage",
       fill="Credit Class") +
       geom_text(aes(label=round(percent,2)),vjust=-0.5,hjust=0) +
  theme_minimal()

# Chi-square test
checking_table=table(cleaned_data$checking_status,cleaned_data$class)
chi_sq_test=chisq.test(checking_table)
print(chi_sq_test)






## Data preparation
# Converting installment_commitment to numeric
cleaned_data$installment_commitment=as.numeric(as.character(cleaned_data$installment_commitment))

# Categorize installment_commitment into ranges
Categorized_installment_commitment=cleaned_data %>% 
  mutate(installment_range=case_when(installment_commitment<=2 ~"Low (<=2)",
                                     installment_commitment>2 & installment_commitment<=4 ~"Medium (2-4)",
                                     installment_commitment>4 ~ "High (>4)"))

# Group data to calculate percentages
Group_data=Categorized_installment_commitment %>% 
  group_by(installment_range,class) %>% 
  summarise(count=n(), .groups='drop') %>% 
  mutate(percentage=count/sum(count)*100)

# Visualizing using Bar Chart
ggplot(Group_data,aes(x=installment_range,y=percentage,fill=class))+
  geom_bar(stat='identity')+
  labs(title='Distribution of Credit Class by Installment Commitment',
       x="Installment Commitment Range",
       y='Percentage',
       fill='Class')+
  theme_minimal()+
  scale_fill_manual(values=c('good'='green','bad'='red'))+
  geom_text(aes(label=percentage,vjust=-1))

# Visualizing using Density plot
ggplot(cleaned_data,aes(x=installment_commitment,fill=class)) +
  geom_density(alpha=0.5) +
  labs(title="Density Plot of Installment Commitment by Credit Class",
       x="Installment Commitment Range",
       y="Density") +
  theme_minimal()

# Performing T-Test(Binary)
t_test=t.test(installment_commitment~class,data=cleaned_data)
print("T-Test for Installment Commitment and Credit Class:")
print(t_test)






# Group data to examine the effect of account_status and installment_commitment on credit_class
Grouped_data2=Categorized_installment_commitment %>%
  group_by(checking_status, installment_range, class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(checking_status, installment_range) %>%
  mutate(percentage = count / sum(count) * 100)

# Visualizing the interaction using a grouped bar chart
ggplot(Grouped_data2,aes(x=checking_status,y=percentage,fill=class)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~installment_range) +
  labs(
    title="Effect of Account Status and Installment Commitment on Credit Classification",
    x="Account Status",
    y="Percentage",
    fill="Credit Class"
  ) +
  theme_minimal() +
  scale_fill_manual(values=c('good'='green','bad'='red'))

## Data Preparation
# Converting credit_class to binary
# Converting checking_status to factor
# Converting installment_commitment to numeric
Group_data3=cleaned_data %>% 
  mutate(class=ifelse(cleaned_data$class=="good",1,0)) %>% 
  mutate(checking_status=as.factor(cleaned_data$checking_status)) %>% 
  mutate(installment_commitment=as.numeric(as.character(cleaned_data$installment_commitment)))

# Logistic regression model
logit_model=glm(class~checking_status + installment_commitment, 
                data=Group_data3, 
                family=binomial)

# Summary of the model
summary(logit_model)

# Calculating odds ratios
exp(coef(logit_model))

# Creating prediction grid
prediction_data=expand.grid(installment_commitment=seq(min(Group_data3$installment_commitment,na.rm=TRUE), 
                                                       max(Group_data3$installment_commitment,na.rm=TRUE),by=0.1),
                            checking_status=levels(Group_data3$checking_status))

# Predict probabilities
prediction_data$predicted_prob=predict(logit_model, 
                                       newdata=prediction_data, 
                                       type="response")

# Plot predictions
ggplot(prediction_data,aes(x=installment_commitment,y=predicted_prob,color=checking_status)) +
  geom_line() +
  labs(title="Predicted Probability of Good Credit Classification",
       x="Installment Commitment",
       y="Predicted Probability") +
  theme_minimal()
