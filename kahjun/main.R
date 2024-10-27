# Load necessary libraries
# install.packages("caret")
library(caret)

# Load the cleaned CSV file
cleaned_data <- read.csv("cleaned_data.csv")

# Check for NA values and empty strings ------------------------------------------------------
na_summary <- sapply(cleaned_data, function(x) sum(is.na(x)))
empty_summary <- sapply(cleaned_data, function(x) sum(x == ""))

summary_df <- data.frame(
  Column = names(cleaned_data),
  NA_Count = na_summary,
  Empty_String_Count = empty_summary
)

print(summary_df)
# --------------------------------------------------------------------------------------------

