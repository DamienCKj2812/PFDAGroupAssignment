library(tidyverse)

data <- read.csv("5. credit_risk_classification.csv")

# Assuming 'data' is a data frame and contains the columns you're interested in
# Selecting specific columns
selected_columns <- data %>%
  select(name, height, ends_with("free"))

# View the selected columns
print(selected_columns)

<<<<<<< HEAD
=======
print("Hello world")
print("hi")
>>>>>>> c3e38e95e3f52df74389a72b37b5fbbe2ea20366
