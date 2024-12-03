
library(ggplot2)
library(dplyr)


#Objective 4: question 1
#create box plot
ggplot(cleaned_data, aes(x = class, y = age, fill = class)) +
  geom_boxplot(alpha = 1) +                                      #adjust color transparency
  labs(title = "Box Plot of Age by Credit Classification",       #add title
       x = "Credit Classification",
       y = "Age") +
  scale_fill_manual(values = c("bad" = "red", "good" = "green")) #assign color

# create a summary of box plot 
summary_stats <- cleaned_data %>%
  group_by(class) %>%                # create a group based on credit classification (class) with the information：
  summarise(Min = min(age),          # min,median,max,1st quartile, 3rd quartile,interquartile range and outliers of age
            Median = median(age),
            Max = max(age),
            Q1 = quantile(age, 0.25),
            Q3 = quantile(age, 0.75),
            IQR = IQR(age),
            Outliers = sum(age < (quantile(age, 0.25) - 1.5 * IQR(age)) | age > (quantile(age, 0.75) + 1.5 * IQR(age))))
print(summary_stats)                 # print results

# Analyze using T-test 
t.test(age ~ class, data = cleaned_data)

#test and train data
set.seed(123)  # For reproducibility/ensure random sampling process produces the same result 
split <- sample(1:nrow(cleaned_data), size = 0.7 * nrow(cleaned_data))  #rondom select data, 70% training, 30% testing
train_data <- cleaned_data[split, ] #create traning data set for 70% training
test_data <- cleaned_data[-split, ] #create testing data set for 30% testing
dim(cleaned_data)                   #display column and row using dim
dim(train_data)
dim(test_data)
table(train_data$class)             # frequency for good and bad in traning data set
table(test_data$credit_history)     #frequency for good and bad in testing data set
train_data$class <- ifelse(train_data$class == "good", 1, 0)   #convert good & bad (character) to 0 & 1(numeric) for logistic regression
test_data$class<-ifelse(test_data$class == "good", 1, 0)       #convert good & bad in credit class to 0 & 1 for consistency

#create logistic regression model to test relationship between age & credit history with class independently. 
model <- glm(class ~ age + credit_history, data = train_data, family = "binomial")
summary(model)
prob_test <- predict(model, newdata = test_data, type = "response")  #show the predicted probabilities for each observation in test_data
conf_matrix <- ifelse(prob_test > 0.5, 1, 0)                         #probability > 0.5, classified as "good", otherwise "bad"
table(Predicted = conf_matrix, Actual = test_data$class)             #table for confusion matrix
accuracy <- mean(conf_matrix == test_data$class)                     #determine the accuracy for tested data
print(paste("Accuracy:", accuracy))                                  #print results



#Objective 4: question 2
#Create a faceted bar plot 
ggplot(cleaned_data, aes(x = class)) +                                         #Set class as x-axis
  geom_bar(aes(fill = class)) +                                                #fill bar chart with "good" and "bad" variable in class
  facet_wrap(~ credit_history) +                                               #separate to different panels for each categories in credit history
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +    #adds the count on top of each bar with position -0.5 above the bar
  labs(title = "Faceted Bar Plot of Credit Classification by Credit History",  #add title
       x = "Credit Classification", y = "Count")

# Analyze using Chi-square test
table_credit_history_class <- table(cleaned_data$credit_history, cleaned_data$class)
chisq.test(table_credit_history_class)



#Objective 4: Question 3
#Create violin plot
ggplot(cleaned_data, aes(x = credit_history, y = age, fill = class)) +                           #fill violin plot using class
  geom_violin() +
  labs(x = "Credit History", y = "Age", title = "Age Distribution by Credit History and Class")  #add title

#Create a logistic regression model with interaction term
interaction_model <- glm(class ~ age * credit_history, data = train_data, family = "binomial")
summary(interaction_model)
prob_test <- predict(interaction_model, newdata=test_data , type = "response")  #show the predicted probabilities for each observation in test_data
conf_matrix_1 <- ifelse(prob_test > 0.5, 1, 0)                                  #probability > 0.5, classified as "good", otherwise "bad"
table(Predicted = conf_matrix_1, Actual = test_data$class)                      #table for confusion matrix
accuracy <- mean(conf_matrix_1 == test_data$class)                              #determine the accuracy for tested data
print(paste("Accuracy:", accuracy))                                             #print results

