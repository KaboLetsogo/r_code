library(readr)
library(tidyverse)
library(corrplot)
library(e1071)
library(stats)

# Importing the data
data <- read_csv("STAT 305 Statistical Computing/STdatasetFinal.csv") 
str(data)
data$Client <- as.factor(data$Client)
data$Paid_This_Month <- as.factor(data$Paid_This_Month)
data$Willingness_to_Pay <- as.factor(data$Willingness_to_Pay)

data$Willingness_to_Pay <- factor(
  data$Willingness_to_Pay,
  levels = c("High", "Medium", "Low")
)
str(data)


numeric_vars <- data %>%
  select(Salary, Amount_Owed, Av_Length_of_Calls, PTPs_Honoured, PTPs_Unhonoured, Installment_Due, AA_Paid)

means_paid_1 <- data %>% 
  filter(Paid_This_Month == 1) %>% 
  summarise(
    Paid = "Yes",
    Mean_Salary = mean(Salary),
    Mean_Amount_Owed = mean(Amount_Owed),
    Mean_Av_Length_of_Calls = mean(Av_Length_of_Calls),
    Mean_PTPs_Honoured = mean(PTPs_Honoured),
    Mean_PTPs_Unhonoured = mean(PTPs_Unhonoured),
    Mean_Installment_Due = mean(Installment_Due),
    Mean_AA_Paid = mean(AA_Paid)
  )

means_paid_0 <- data %>% 
  filter(Paid_This_Month == 0) %>% 
  summarise(
    Paid = "No",
    Mean_Salary = mean(Salary),
    Mean_Amount_Owed = mean(Amount_Owed),
    Mean_Av_Length_of_Calls = mean(Av_Length_of_Calls),
    Mean_PTPs_Honoured = mean(PTPs_Honoured),
    Mean_PTPs_Unhonoured = mean(PTPs_Unhonoured),
    Mean_Installment_Due = mean(Installment_Due),
    Mean_AA_Paid = mean(AA_Paid)  # This would be 0 since Paid == 0
  )

# Combine the results into a single data frame for comparison
combined_means <- bind_rows(means_paid_1 = means_paid_1, means_paid_0 = means_paid_0)
print(combined_means)

# Calculate medians for the subset where Paid == 1
medians_paid_1 <- data %>% 
  filter(Paid_This_Month == 1) %>% 
  summarise(
    Paid = "Yes",
    Median_Salary = median(Salary, na.rm = TRUE),
    Median_Amount_Owed = median(Amount_Owed, na.rm = TRUE),
    Median_Av_Length_of_Calls = median(Av_Length_of_Calls, na.rm = TRUE),
    Median_PTPs_Honoured = median(PTPs_Honoured, na.rm = TRUE),
    Median_PTPs_Unhonoured = median(PTPs_Unhonoured, na.rm = TRUE),
    Median_Installment_Due = median(Installment_Due, na.rm = TRUE),
    Median_AA_Paid = median(AA_Paid, na.rm = TRUE)
  )

# Calculate medians for the subset where Paid == 0
medians_paid_0 <- data %>% 
  filter(Paid_This_Month == 0) %>% 
  summarise(
    Paid = "No",
    Median_Salary = median(Salary, na.rm = TRUE),
    Median_Amount_Owed = median(Amount_Owed, na.rm = TRUE),
    Median_Av_Length_of_Calls = median(Av_Length_of_Calls, na.rm = TRUE),
    Median_PTPs_Honoured = median(PTPs_Honoured, na.rm = TRUE),
    Median_PTPs_Unhonoured = median(PTPs_Unhonoured, na.rm = TRUE),
    Median_Installment_Due = median(Installment_Due, na.rm = TRUE),
    Median_AA_Paid = median(AA_Paid, na.rm = TRUE)  # This would be 0 since Paid == 0
  )

# Combine the results into a single data frame for comparison
combined_medians <- bind_rows(medians_paid_1 = medians_paid_1, medians_paid_0 = medians_paid_0)
print(combined_medians)


# Assuming 'data' is your DataFrame
numeric_vars <- select(data, Salary, Amount_Owed, Av_Length_of_Calls, PTPs_Honoured, PTPs_Unhonoured, Installment_Due, AA_Paid)

# Calculate skewness and kurtosis
skewness_values <- sapply(numeric_vars, skewness, na.rm = TRUE)
kurtosis_values <- sapply(numeric_vars, kurtosis, na.rm = TRUE)

# Combine and print the results
summary_stats <- data.frame(
  Variable = names(numeric_vars),
  Skewness = skewness_values,
  Kurtosis = kurtosis_values
)

print(summary_stats)


summary_stats <- data %>%
  summarise(
    SD_Salary = sd(Salary, na.rm = TRUE),
    SD_Amount_Owed = sd(Amount_Owed, na.rm = TRUE),
    SD_Av_Length_of_Calls = sd(Av_Length_of_Calls, na.rm = TRUE),
    SD_PTPs_Honoured = sd(PTPs_Honoured, na.rm = TRUE),
    SD_PTPs_Unhonoured = sd(PTPs_Unhonoured, na.rm = TRUE),
    SD_Installment_Due = sd(Installment_Due, na.rm = TRUE),
    SD_Amount_Actually_Paid = sd(AA_Paid, na.rm = TRUE),
    IQR_Salary = IQR(Salary, na.rm = TRUE),
    IQR_Amount_Owed = IQR(Amount_Owed, na.rm = TRUE),
    IQR_Av_Length_of_Calls = IQR(Av_Length_of_Calls, na.rm = TRUE),
    IQR_PTPs_Honoured = IQR(PTPs_Honoured, na.rm = TRUE),
    IQR_PTPs_Unhonoured = IQR(PTPs_Unhonoured, na.rm = TRUE),
    IQR_Installment_Due = IQR(Installment_Due, na.rm = TRUE),
    IQR_Amount_Actually_Paid = IQR(AA_Paid, na.rm = TRUE)
  )

# Print the summary statistics
print(summary_stats)

str(data)

numeric_vars <- data[, c("Salary", "Amount_Owed", "Av_Length_of_Calls", "PTPs_Honoured", "PTPs_Unhonoured", "Installment_Due", "AA_Paid")]
cor_matrix <- cor(numeric_vars)
print(cor_matrix)


# Assuming 'data' is your dataset
numeric_columns <- select(data, where(is.numeric))

# Apply the Shapiro-Wilk test to each numeric column and store results with column names
shapiro_results <- sapply(numeric_columns, function(x) {
  shapiro_test <- shapiro.test(x)
  return(shapiro_test$p.value)
}, simplify = FALSE)

# Convert the results to a data frame
shapiro_results_df <- as.data.frame(shapiro_results, stringsAsFactors = FALSE)
colnames(shapiro_results_df) <- names(numeric_columns)

# Transpose the data frame for better readability
shapiro_results_df <- t(shapiro_results_df)
colnames(shapiro_results_df) <- "P-Value"

# Print the results
print(shapiro_results_df)

# Scatter plot for Salary vs. Installment Due
ggplot(data, aes(x = Salary, y = Installment_Due)) +
  geom_point(aes(color = Paid_This_Month), alpha = 0.6) +
  labs(title = "Scatter Plot: Salary vs. Installment Due",
       x = "Salary",
       y = "Installment Due",
       color = "Paid This Month") +
  theme_minimal()

# Box plot for Salary by Paid This Month status
ggplot(data, aes(x = as.factor(Paid_This_Month), y = Salary)) +
  geom_boxplot(aes(fill = as.factor(Paid_This_Month))) +
  labs(title = "Box Plot: Salary by Paid This Month",
       x = "Paid This Month",
       y = "Salary",
       fill = "Paid This Month") +
  theme_minimal()





# Bar Chart for Client and Paid_This_Month
  ggplot(data, aes(x=Client, fill=factor(Paid_This_Month))) +
  geom_bar(position="dodge") +
  labs(title="Client vs. Paid This Month", x="Client", fill="Paid This Month") +
  facet_wrap(~Paid_This_Month)


  
  table_willingness_paid <- table(data$Willingness_to_Pay, data$Paid_This_Month)
  
  # Create a data frame for plotting
  data_for_plot <- as.data.frame(table_willingness_paid)
  names(data_for_plot) <- c("Willingness_to_Pay", "Paid_This_Month", "Count")
  
  # Adjusting the dataframe for plotting percentages
  data_for_plot <- data_for_plot %>%
    group_by(Paid_This_Month) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Generate the pie chart
  ggplot(data_for_plot, aes(x = "", y = Percentage, fill = factor(Willingness_to_Pay))) +
    geom_bar(stat = "identity", width = 1) +
    facet_wrap(~Paid_This_Month, labeller = label_both) +
    coord_polar(theta = "y") +
    labs(title = "Pie Chart: Willingness to Pay vs Paid This Month",
         y = NULL, x = NULL, fill = "Willingness to Pay") +
    theme_void() +
    theme(legend.position = "bottom")
  
  # Perform the chi-squared test
  chi_squared_test <- chisq.test(table_willingness_paid)
  print(chi_squared_test)  
  
  # Cross-tabulation of Client and PTP This Month
  client_ptptm_tab <- table(data$Client, data$PTP_This_Month)
  client_ptptm_tab
   chisq_client_ptptm <- chisq.test(client_ptptm_tab)  
   chisq_client_ptptm

# Logistic Regression for PTP This Month
model_ptp <- glm(PTP_This_Month ~ Salary + Amount_Owed + Av_Length_of_Calls + PTPs_Honoured + PTPs_Unhonoured + Installment_Due, 
                 data = data, family = binomial())
summary(model_ptp)

# Logistic Regression for Paid This Month (All Data)
model_paid_all <- glm(Paid_This_Month ~ Salary + Amount_Owed + Av_Length_of_Calls + PTPs_Honoured + PTPs_Unhonoured + Installment_Due, 
                      data = data, family = binomial())
summary(model_paid_all)

# Logistic Regression for Paid This Month (Only for Debtors with a PTP)
data_with_ptp <- subset(data, PTP_This_Month == 1)
model_paid_ptp <- glm(Paid_This_Month ~ Salary + Amount_Owed + Av_Length_of_Calls + PTPs_Honoured + PTPs_Unhonoured + Installment_Due, 
                      data = data_with_ptp, family = binomial())
summary(model_paid_ptp)


