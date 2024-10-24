data <- read.csv("5. credit_risk_classification.csv")
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

#View NA value
View(data[is.na(data$employment), ])

#----------------CLEAN EMPLOYMENT------------------------
#Clean NA in employment
getmode=function(v){
  uniqv<- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(data$employment)
data$employment=replace(data$employment, is.na(data$employment), getmode(data$employment))
table(data$employment)

#See unique value
unique(data$employment)
View(data[is.na(data$credit_amount), ])

#----------------CLEAN EMPLOYMENT------------------------

#-------------------CLEAN CREDIT_AMOUNT------------------------

#Clean NA in credit_amount
data$credit_amount[is.na(data$credit_amount)] <- mean(data$credit_amount, na.rm = TRUE)
# Round the credit_amount column to 2 decimal places
data$credit_amount <- round(data$credit_amount, 2)
# Format credit_amount to show two decimal places using sprintf
data$credit_amount <- sprintf("%.2f", data$credit_amount)

#-------------------CLEAN CREDIT_AMOUNT------------------------

#-------------------CLEAN RESIDENCE_SINCE------------------------

unique(data$residence_since)
data$class <- as.factor(data$class)

#See is there any NA 
View(data[is.na(data$residence_since), ])
View(data)
# Round residence_since values to 2 decimal points
data$residence_since <- round(data$residence_since, 2)
data$residence_since <- sprintf("%.2f", data$residence_since)

# View the first few rows to verify changes
head(data$residence_since)

unique(data$residence_since)

#Clean NA value in residence_since using mean
data$residence_since[is.na(data$residence_since)] <- mean(data$residence_since, na.rm = TRUE)

#-------------------CLEAN RESIDENCE_SINCE------------------------

#-------------------CLEAN NUM_DEPENDENTS---------------------------

unique(data$num_dependents)

# Round values: less than 0.5 rounds down, 0.5 or more rounds up
data$num_dependents <- round(data$num_dependents)

# Replace NA values with 0
data$num_dependents[is.na(data$num_dependents)] <- 0

#Replace NA values with mode in own_telephone
unique(data$own_telephone)
getmode(data$own_telephone)
data$own_telephone=replace(data$own_telephone, is.na(data$own_telephone), getmode(data$own_telephone))

#Replace NA values with mode in foreign_worker
unique(data$foreign_worker)
getmode(data$foreign_worker)
data$foreign_worker=replace(data$foreign_worker, is.na(data$foreign_worker), getmode(data$foreign_worker))

unique(data$class)

#-------------------CLEAN NUM_DEPENDENTS---------------------------








