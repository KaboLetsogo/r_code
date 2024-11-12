library(tidyverse)
library(corrplot)  # For the correlation plot
library(ggplot2)   # For data visualization

# Set seed for reproducibility
set.seed(123)

# Generate unique Debtor_IDs
debtor_ids <- sample(3000:9000, 100, replace = FALSE)  # 100 unique IDs between 3000 and 9000

# Generate base data frame
months <- seq(as.Date("2023-05-01"), as.Date("2023-12-01"), by="month")

# Create the dataset using the unique Debtor_IDs
data <- expand.grid(ForMonth=months, Debtor_ID=debtor_ids) %>%
  mutate(
    Client = sample(c("BPC", "WUC", "Letshego", "Absa"), nrow(.), replace=TRUE, prob=c(0.25, 0.25, 0.25, 0.25)),
    Salary = round(runif(nrow(.), min=20000, max=80000), 0),  # Randomly generated salaries
    Amount_Owed = round(runif(nrow(.), min=1000, max=100000), 0),  # Random debts between 1K and 100K
    Av_Length_of_Calls = round(runif(nrow(.), min=1, max=20), 0),  # Call lengths between 1 and 20 mins
    PTP_This_Month = sample(c(0, 1), nrow(.), replace=TRUE)  # Binary, whether a PTP was made
  ) %>%
  group_by(Debtor_ID) %>%
  mutate(
    PTPs_Honoured = cumsum(lag(PTP_This_Month, default=0)),  # Cumulative count of honoured PTPs
    PTPs_Unhonoured = round(Amount_Owed/10000) - PTPs_Honoured,  # Derived from Amount Owed
    Installment_Due = ifelse(PTP_This_Month == 1, round(runif(nrow(.), min=500, max=8000), 0), 0),  # Installments due if PTP made
    Paid_This_Month = ifelse(PTP_This_Month == 1, sample(c(0, 1), nrow(.), replace=TRUE), 0)  # Payment status for the month
  ) %>%
  ungroup() %>%
  mutate(
    Willingness_to_Pay = case_when(
      Av_Length_of_Calls > 15 & Salary > median(Salary[Client == Client], na.rm = TRUE) & PTPs_Honoured >= median(PTPs_Honoured, na.rm = TRUE) ~ "High",
      Av_Length_of_Calls > 10 & Salary > median(Salary) ~ "Medium",
      TRUE ~ "Low"
    )  # Derived Willingness to Pay based on other variables
  )



data <- data %>%
  mutate(
    # Adjusting PTPs based on Salary
    PTPs_Honoured = if_else(Salary > 60000, PTPs_Honoured + round(runif(n(), min=1, max=3)), PTPs_Honoured),
    #PTPs_Unhonoured = if_else(Salary > 60000, PTPs_Unhonoured - round(runif(n(), min=1, max=3)), PTPs_Unhonoured),
    # Recalculating Installment Due to reflect Salary impact
    Installment_Due = if_else(Salary > 60000, Installment_Due * 1.1, Installment_Due)
  )


data <- data %>%
  mutate(
    # Assign "High" willingness to pay more often for WUC and BPC
    Willingness_to_Pay = case_when(
      Client %in% c("WUC", "BPC") & runif(n()) < 0.6 ~ "High",  # 0.7
      Client %in% c("Letshego") & runif(n()) < 0.4 ~ "Medium",  # 0.5
      Client %in% c("Absa") & runif(n()) < 0.333 ~ "Low", # 0.3
      TRUE ~ sample(c("Low", "Medium", "High"), 1, replace = TRUE, prob = c(0.4, 0.4, 0.2))  # Base case
    )
  )

# Chi-squared test for Willingness to Pay vs. Client
data$Willingness_to_Pay <- factor(
  data$Willingness_to_Pay,
  levels = c("High", "Medium", "Low")
)

table_wp_client <- table(data$Willingness_to_Pay, data$Client)
chisq_test <- chisq.test(table_wp_client)

# Print results
print(chisq_test)

# Calculate and examine standardized residuals
std_residuals <- chisq_test$residuals
significant_cells <- which(abs(std_residuals) > 2, arr.ind = TRUE)

# Print out cells with significant residuals
if (length(significant_cells) > 0) {
  print("Cells with significant residuals:")
  print(significant_cells)
} else {
  print("No significant residuals found.")
}

# Optional: Visualize the residuals
residuals_data <- as.data.frame(table_wp_client)
names(residuals_data) <- c("Willingness", "Client", "Freq")
residuals_data$Std_Residuals <- as.vector(std_residuals)

ggplot(residuals_data, aes(x=Willingness, y=Client, fill=Std_Residuals)) +
  geom_tile() +
  geom_text(aes(label=sprintf("%.2f", Std_Residuals)), color="white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-12, 12), name="Standardized\nResiduals") +
  labs(title="Heatmap of Standardized Residuals", x="Willingness to Pay", y="Client")


generate_aa_paid <- function(paid_this_month, willingness_to_pay, installment_due) {
  if (paid_this_month == 0) {
    return(0)
  }
  random_chance <- runif(1)
  random_payment <- runif(1)
  if (willingness_to_pay == "High" && random_chance < 0.7) {
    return(runif(1, min = 0.75, max = 1) * installment_due)
  } else if (willingness_to_pay == "Medium" && random_chance < 0.7) {
    return(runif(1, min = 0.4, max = 0.7) * installment_due)
  } else if (willingness_to_pay == "Low" && random_chance < 0.7) {
    return(runif(1, min = 0, max = 0.4) * installment_due)
  } else {
    return(random_payment * installment_due)
  }
}

# Add AA Paid column to data frame
data <- data %>%
  rowwise() %>%
  mutate(
    AA_Paid = generate_aa_paid(Paid_This_Month, Willingness_to_Pay, Installment_Due)
  ) %>%
  ungroup()

write.csv(data, 'STAT 305 Statistical Computing/STdatasetFinal1.csv', row.names = FALSE)

numeric_vars <- data %>% select(Salary, Amount_Owed, Av_Length_of_Calls, PTPs_Honoured, PTPs_Unhonoured, Installment_Due,AA_Paid)
cor(numeric_vars)

data <- select(data, -Debtor_ID, -ForMonth)

# Save the modified data back to CSV
write_csv(data, "STAT 305 Statistical Computing/STdatasetFinal2.csv")
library(readr)
data <- read_csv("STAT 305 Statistical Computing/STdatasetFinal2.csv") 
set.seed(123)  # For reproducibility

data <- data %>%
  mutate(Salary = Salary * 2.67)

# Assuming 'data' has the 'PTP_This_Month' with values 0 or 1
data <- data %>%
  mutate(
    Total_PTPs = sample(7:14, n(), replace = TRUE),  # Random total PTPs between 7 and 14
    Negator = ifelse(PTP_This_Month == 1, 
                     ifelse(runif(n()) < 0.35, -1, 1), 
                     ifelse(runif(n()) < 0.65, -1, 1)),
    Modifier = runif(n(), 0, 0.4) * Negator,
    Base_Proportion = 0.5,
    Prop_Honoured = pmax(0, pmin(1, Base_Proportion + Modifier)),  # Ensuring proportions are between 0 and 1
    PTPs_Honoured = round(Total_PTPs * Prop_Honoured),
    PTPs_Unhonoured = Total_PTPs - PTPs_Honoured
  )

# Ensuring no negative values for honoured and unhonoured PTPs
data <- data %>%
  mutate(
    PTPs_Honoured = pmax(0, PTPs_Honoured),
    PTPs_Unhonoured = pmax(0, PTPs_Unhonoured)
  )%>%
  select(-Total_PTPs, -Negator, -Modifier, -Base_Proportion, -Prop_Honoured)

# View the first few rows to check results
View(data)
write_csv(data, "STAT 305 Statistical Computing/STdatasetFinal2.csv")
