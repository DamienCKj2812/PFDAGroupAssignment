data <- read.csv("5. credit_risk_classification.csv")
library(ggplot)
data
# Assuming 'data' is a data frame and contains the columns you're interested in
# Selecting specific columns
selected_columns <- data %>%
  select(name, height, ends_with("free"))

# View the selected columns
print(selected_columns)

View(data)

#C lean NA in employment
View(df[is.na(df$employment), ])
getmode=function(v){
  uniqv<- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(df$employment)
df$employment=replace(df$employment, is.na(df$employment), getmode(df$employment))
table(df$employment)

#See unique value
unique(df$employment)

#Clean NA in credit_amount
View(df[is.na(df$credit_amount), ])
df$credit_amount[is.na(df$credit_amount)] <- mean(df$credit_amount, na.rm = TRUE)

# Load necessary libraries


# Round the credit_amount column to 2 decimal places
df$credit_amount <- round(df$credit_amount, 2)
View(data)

