# Load necessary libraries
# install.packages("VIM")
# install.packages("caret")
# install.packages("RANN")
library(VIM)
library(caret)
library(RANN)
library(tidyr)
library(dplyr)
library(mice)
library(magrittr)

data <- read.csv("../5. credit_risk_classification.csv")

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
  if (!(column_name %in% names(data))) stop(paste("Error: Column", column_name, "does not exist."))
  data[[column_name]] <- as.factor(data[[column_name]])
  imputation_methods <- make.method(data)
  imputation_methods[column_name] <- "polyreg"
  imputed_data <- mice(data, method = imputation_methods, m = m)
  completed_data <- complete(imputed_data, 1)
  return(completed_data)
}

replace_empty_with_na <- function(data, column_name) {
  if (!column_name %in% names(data)) stop(paste("Column", column_name, "does not exist."))
  data[[column_name]][data[[column_name]] == ""] <- NA
  return(data)
}

knn_imputation <- function(data, target_column, class_column, k_value = 10) {
  df <- data.frame(credit_hist = data[[target_column]], credit_class = data[[class_column]])
  df$credit_hist <- as.factor(df$credit_hist)
  imputed_data <- kNN(df, variable = "credit_hist", k = k_value)
  data[[target_column]] <- imputed_data$credit_hist
  return(data)
}

manual_hot_deck_imputation <- function(data, age_column, employment_column) {
  df <- data.frame(age = data[[age_column]], years_of_employment = data[[employment_column]])
  categorize_employment_years <- function(years) {
    if (is.na(years)) return(NA)
    else if (years == "unemployed") return("Unemployment")
    else if (years == "<1") return("0 to 1 year")
    else if (years == "1<=X<4") return("1 to 4 years")
    else if (years == "4<=X<7") return("4 to 7 years")
    else if (years == ">=7") return("More than 7 years")
  }
  df$employment_category <- sapply(df$years_of_employment, categorize_employment_years)
  for (index in which(is.na(df$age))) {
    donors <- df[!is.na(df$years_of_employment), ]
    if (nrow(donors) == 0) next
    donor_row <- donors[sample(nrow(donors), 1), ]
    if (donor_row$years_of_employment == "unemployed") {
      df$age[index] <- floor(mean(data[[age_column]][data[[employment_column]] == "unemployed"], na.rm = TRUE))
    } else if (donor_row$years_of_employment %in% c("<1", "1<=X<4", "4<=X<7")) {
      df$age[index] <- floor(mean(data[[age_column]][data[[employment_column]] == "<1"], na.rm = TRUE))
    } else if (donor_row$years_of_employment == ">=7") {
      df$age[index] <- floor(mean(data[[age_column]][data[[employment_column]] == ">=7"], na.rm = TRUE))
    }
  }
  data[[age_column]] <- df$age
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
  replace_empty_with_na("personal_status") %>%
  mice_imputation("personal_status") %>%
  replace_empty_with_na("other_parties") %>%
  mice_imputation("other_parties") %>%
  replace_empty_with_na("residence_since") %>%
  mean_median_mode_imputation("residence_since", method = "mean") %>%
  replace_empty_with_na("property_magnitude") %>%
  mice_imputation("property_magnitude") %>%
  replace_empty_with_na("age")  %>%
  manual_hot_deck_imputation("age", "employment") %>%
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


