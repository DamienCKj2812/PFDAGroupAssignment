# Load necessary libraries
# install.packages("VIM")
# install.packages("caret")
# install.packages("RANN")
library(VIM)
library(caret)
library(RANN)

# KNN Imputation Function
knn_imputation <- function(data, target_column, class_column, k_value = 10) {
  
  # Create a temporary dataset with target and class columns
  df <- data.frame(
    credit_hist = data[[target_column]],
    credit_class = data[[class_column]]
  )
  
  # Ensure target_column is a factor (required for KNN imputation)
  df$credit_hist <- as.factor(df$credit_hist)
  
  # Apply KNN imputation
  imputed_data <- kNN(df, variable = "credit_hist", k = k_value)
  
  # Update the original dataset with imputed values
  data[[target_column]] <- imputed_data$credit_hist
  
  return(data)
}

# Usage:
# data <- knn_imputation(data, "credit_history", "class", k_value = 10)


# Manual Hot Deck Imputation Function for Age
manual_hot_deck_imputation <- function(data, age_column, employment_column) {
  
  # Create a temporary dataset with age and employment columns
  df <- data.frame(
    age = data[[age_column]],
    years_of_employment = data[[employment_column]]
  )
  
  # Categorize employment years
  categorize_employment_years <- function(years) {
    if (is.na(years)) {
      return(NA)
    } else if (years == "unemployed") {
      return("Unemployment")
    } else if (years == "<1") {
      return("0 to 1 year")
    } else if (years == "1<=X<4") {
      return("1 to 4 years")
    } else if (years == "4<=X<7") {
      return("4 to 7 years")
    } else if (years == ">=7") {
      return("More than 7 years")
    }
  }
  
  # Apply categorization
  df$employment_category <- sapply(df$years_of_employment, categorize_employment_years)
  
  # Function for hot deck imputation based on employment duration
  for (index in which(is.na(df$age))) {
    donors <- df[!is.na(df$years_of_employment), ]
    if (nrow(donors) == 0) next
    
    donor_row <- donors[sample(nrow(donors), 1), ]
    
    # Impute based on employment category
    if (donor_row$years_of_employment == "unemployed") {
      df$age[index] <- floor(mean(data[[age_column]][data[[employment_column]] == "unemployed"], na.rm = TRUE))
    } else if (donor_row$years_of_employment %in% c("<1", "1<=X<4", "4<=X<7")) {
      df$age[index] <- floor(mean(data[[age_column]][data[[employment_column]] == "<1"], na.rm = TRUE))
    } else if (donor_row$years_of_employment == ">=7") {
      df$age[index] <- floor(mean(data[[age_column]][data[[employment_column]] == ">=7"], na.rm = TRUE))
    }
  }
  
  # Replace original age data with imputed values
  data[[age_column]] <- df$age
  
  return(data)
}

# Usage:
# data <- manual_hot_deck_imputation(data, "age", "employment")
