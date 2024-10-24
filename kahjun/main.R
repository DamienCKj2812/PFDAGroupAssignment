library(tidyr)
library(dplyr)
library(mice)

data <- read.csv("./5. credit_risk_classification.csv")

# Function to remove rows with high percentages of NA values
remove_high_na_rows <- function(data, threshold = 70) {
  # Calculate percentage of NA values per row
  na_percentage <- rowMeans(is.na(data)) * 100
  
  # Remove rows where NA percentage exceeds the threshold
  cleaned_data <- data[na_percentage <= threshold, ]
  
  return(cleaned_data)
}
mean_median_imputation  <- function(data, column_name, method = c("mean", "median")) {
  # Match the method input to either "mean" or "median"
  method <- match.arg(method)
  
  # Extract the specified column
  column <- data[[column_name]]
  
  # Check for negative values in the column
  if (any(column < 0, na.rm = TRUE)) {
    cat("Warning: There are negative values in '", column_name, "'.\n", sep = "")
    return(data)  # Optionally return early to prevent further processing
  }
  
  highest_value <- max(column, na.rm = TRUE)
  lowest_value <- min(column, na.rm = TRUE)
  
  cat("The highest value in '", column_name, "' is: ", highest_value, "\n", sep = "")
  cat("The lowest value in '", column_name, "' is: ", lowest_value, "\n", sep = "")
  
  if (any(is.na(column))) {
    # Calculate the mean or median of the column, excluding NA values
    if (method == "mean") {
      imputed_value <- mean(column, na.rm = TRUE)
      cat("Missing values in '", column_name, "' have been filled with the mean value: ", imputed_value, "\n", sep = "")
    } else {
      imputed_value <- median(column, na.rm = TRUE)
      cat("Missing values in '", column_name, "' have been filled with the median value: ", imputed_value, "\n", sep = "")
    }
    
    # Replace NA values in the column with the calculated value
    data[[column_name]][is.na(column)] <- imputed_value
  } else {
    cat("No missing values found in '", column_name, "'. No imputation needed.\n", sep = "")
  }
  
  return(data)
}


mice_imputation <- function(data, column_name, m = 5) {
  # Check if the column exists in the dataset
  if (!(column_name %in% names(data))) {
    stop(paste("Error: The column", column_name, "does not exist in the dataset."))
  }
  
  # Extract the specified column as a vector
  column <- data[[column_name]]
  
  # Check the NA count before imputation
  na_count_before <- sum(is.na(column))
  message("NA count before imputation: ", na_count_before)
  
  # Ensure the column is a factor
  data[[column_name]] <- as.factor(column)
  
  # Specify imputation methods
  imputation_methods <- make.method(data)
  imputation_methods[column_name] <- "polyreg"  # Use polyreg for categorical variables
  
  # Run the mice algorithm with the specified number of imputations
  imputed_data <- mice(data, method = imputation_methods, m = m)
  
  # Check the imputed values for the specified column
  message("Imputed values for the column: ", column_name)
  print(imputed_data$imp[[column_name]])
  
  # Extract the completed dataset (after imputation)
  completed_data <- complete(imputed_data, 1)
  
  # Check the NA count after imputation
  na_count_after <- sum(is.na(completed_data[[column_name]]))
  message("NA count after imputation: ", na_count_after)
  
  # Return the completed dataset
  return(completed_data)
}

replace_empty_with_na <- function(data, column_name) {
  # Check if the column exists in the dataframe
  if (!column_name %in% names(data)) {
    stop(paste("Column", column_name, "does not exist in the data frame."))
  }
  
  # Extract the specified column
  column <- data[[column_name]]
  
  # Replace empty strings with NA
  data[[column_name]][column == ""] <- NA
  
  cat("Empty strings in '", column_name, "' have been replaced with NA.\n", sep = "")
  
  return(data)
}

cleaned_data <- data %>%
  remove_high_na_rows() %>%
  replace_empty_with_na("duration") %>%
  mean_median_imputation("duration", method = "mean") %>%
  replace_empty_with_na("purpose") %>%
  mice_imputation("purpose") %>%
  replace_empty_with_na("savings_status") %>%
  mice_imputation("savings_status") %>%
  replace_empty_with_na("personal_status") %>%
  mice_imputation("personal_status") %>%
  replace_empty_with_na("other_parties") %>%
  mice_imputation("other_parties") %>%
  replace_empty_with_na("property_magnitude") %>%
  mice_imputation("property_magnitude") %>%
  replace_empty_with_na("other_payment_plans") %>%
  mice_imputation("other_payment_plans") %>%
  replace_empty_with_na("housing") %>%
  mice_imputation("housing") %>%
  replace_empty_with_na("existing_credits") %>%
  mean_median_imputation("existing_credits", method = "median") %>%
  replace_empty_with_na("job") %>%
  mice_imputation("job")

# Check the cleaned data
head(cleaned_data)





