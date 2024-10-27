library(tidyr)
library(dplyr)
library(mice)
library(DataExplorer)
library(nnet)
library(VIM)



### Import Data
fileUrl= "C:\\Users\\user\\Desktop\\Programing for DA\\Assignment\\5. credit_risk_classification.csv"

data=read.csv(fileUrl)

summary(data)
str(data)


## Identifying missing value
plot_missing(data)


# Function to remove rows with high percentages of NA values
remove_high_na_rows <- function(data, threshold = 70) {
  # Calculate percentage of NA values per row
  na_percentage <- rowMeans(is.na(data)) * 100
  
  # Remove rows where NA percentage exceeds the threshold
  cleaned_data <- data[na_percentage <= threshold, ]
  
  return(cleaned_data)
}

# Removing high NA percentage rows
remove_high_na_rows(data)

# Identifying Row that consist NA Value
data %>% 
  select(checking_status,installment_commitment) %>% 
  filter(!complete.cases(.))


### Imputing 'Checking_Status'
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

##Saving imputed 'Checking_Status'
Cleanned_data=mice_imputation(data,'checking_status')


###Imputing 'installment_commitment'
# Performing KNN imputation
data_knn=kNN(data, variable = "installment_commitment", k = 5)

# Checking imputed column
data$installment_commitment_knn=data_knn$installment_commitment
data$installment_commitment

# Replacing the data to cleaned_data 
Cleanned_data$installment_commitment=data$installment_commitment_knn
Cleanned_data

# Ensuring no NA
sum(is.na(Cleanned_data$installment_commitment))
