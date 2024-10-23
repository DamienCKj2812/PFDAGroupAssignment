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
df$employment=replace(df$furnishing, is.na(df$furnishing), getmode(df$furnishing))
table(df$furnishing)