library(ggplot2)

# Load the cleaned CSV file
data <- read.csv("cleaned_data.csv", stringsAsFactors = TRUE)

str(data)
  
  # Frequency table for savings status and credit class
freq_table <- table(data$savings_status, data$class)
print(freq_table)

# Create a stacked bar plot for visual comparison
ggplot(data, aes(x = savings_status, fill = class)) +
  geom_bar(position = "fill") +  # "fill" for proportionate stacking
  ylab("Proportion") +
  labs(title = "Credit Class Distribution by Savings Status")



