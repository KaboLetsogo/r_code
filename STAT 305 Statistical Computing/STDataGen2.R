library(dplyr)
library(readr)

set.seed(130)  # For reproducibility

# Load your existing dataset
data <- read_csv("STAT 305 Statistical Computing/STdatasetFinal2.csv")

# Define a function to assign quartile rankings
get_quartile <- function(x) {
  ntile(x, 4)
}

# Apply the quartile function to each relevant column
data <- data %>%
  mutate(
    Salary_Quartile = get_quartile(Salary),
    Honoured_Quartile = get_quartile(PTPs_Honoured),
    Unhonoured_Quartile = get_quartile(PTPs_Unhonoured),
    AmountOwed_Quartile = get_quartile(Amount_Owed)
  )

# Create lists for each variable based on their own quartiles
quartile_lists <- list(
  Salary = split(data$Salary, data$Salary_Quartile),
  Honoured = split(data$PTPs_Honoured, data$Honoured_Quartile),
  Unhonoured = split(data$PTPs_Unhonoured, data$Unhonoured_Quartile),
  AmountOwed = split(data$Amount_Owed, data$AmountOwed_Quartile)
)

# Initialize data2
data2 <- data.frame(Salary = data$Salary)
data2$Honoured <- NA
data2$Unhonoured <- NA
data2$Amount_Owed <- NA

# Define quartile range functions to handle specific quartiles and the full range
get_quartile_range <- function(quartile, var) {
  quantiles <- quantile(var, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  # Handle the case where the full range of the variable is needed
  if (quartile == 5) {
    return(c(quantiles[1], quantiles[5]))  # Returns the range for the entire data
  }
  # Return the range for the specified quartile
  return(c(quantiles[quartile], quantiles[quartile + 1]))
}

# Populate data2 with loop through rows
for (i in 1:nrow(data2)) {
  salary_q <- data$Salary_Quartile[i]
  
  # Select Honoured based on random chance and quartile
  if (runif(1) <= 0.7) {
    honoured_range <- get_quartile_range(salary_q, data$PTPs_Honoured)
  } else {
    honoured_range <- get_quartile_range(5, data$PTPs_Honoured)  # Use full range if rand > 0.8
  }
  data2$Honoured[i] <- runif(1, honoured_range[1], honoured_range[2])
  
  # Determine the opposing quartile for Unhonoured
  opposite_q <- ifelse(salary_q == 1, 4, ifelse(salary_q == 2, 3, ifelse(salary_q == 3, 2, 1)))
  
  # Select Unhonoured from the opposing quartile
  if (runif(1) <= 0.6) {
   unhonoured_range <- get_quartile_range(opposite_q, data$PTPs_Unhonoured)
  } else {
    unhonoured_range <- get_quartile_range(5, data$PTPs_Unhonoured)  
  }
   data2$Unhonoured[i] <- runif(1, unhonoured_range[1], unhonoured_range[2])
  
  # Select Amount Owed based on random chance and quartile of Unhonoured
  if (runif(1) <= 0.6) {
    amount_owed_range <- get_quartile_range(opposite_q, data$Amount_Owed)
  } else {
    amount_owed_range <- get_quartile_range(5, data$Amount_Owed)  # Use full range if rand > 0.65
  }
  data2$Amount_Owed[i] <- runif(1, amount_owed_range[1], amount_owed_range[2])
}


# Calculate correlation among numeric variables in data2
cor_data2 <- cor(data2[, c("Salary", "Honoured", "Unhonoured", "Amount_Owed")])
print(cor_data2)

# Ensure Honoured and Unhonoured are integers
data2$Honoured <- as.integer(data2$Honoured)
data2$Unhonoured <- as.integer(data2$Unhonoured)

# Initialize the new columns
data2$Willingness_to_Pay <- ""
data2$Client <- ""

# Define the function to set Willingness to Pay and Client based on Honoured quartile
set_values_based_on_quartile <- function(honoured_quartile, rand_wtp, rand_client) {
  if (honoured_quartile == 4) {
    return(list(
      Willingness_to_Pay = ifelse(rand_wtp <= 0.15, "Low", ifelse(rand_wtp <= 0.4, "Medium", "High")),
      Client = ifelse(rand_client <= 0.35, "WUC", ifelse(rand_client <= 0.7, "BPC", ifelse(rand_client <= 0.85, "Letshego", "Absa")))
    ))
  } else if (honoured_quartile == 3) {
    return(list(
      Willingness_to_Pay = ifelse(rand_wtp <= 0.2, "Low", ifelse(rand_wtp <= 0.55, "Medium", "High")),
      Client = ifelse(rand_client <= 0.3, "WUC", ifelse(rand_client <= 0.6, "BPC", ifelse(rand_client <= 0.8, "Letshego", "Absa")))
    ))
  } else if (honoured_quartile == 2) {
    return(list(
      Willingness_to_Pay = ifelse(rand_wtp <= 0.4, "Low", ifelse(rand_wtp <= 0.75, "Medium", "High")),
      Client = ifelse(rand_client <= 0.25, "WUC", ifelse(rand_client <= 0.5, "BPC", ifelse(rand_client <= 0.75, "Letshego", "Absa")))
    ))
  } else { # honoured_quartile == 1
    return(list(
      Willingness_to_Pay = ifelse(rand_wtp <= 0.45, "Low", ifelse(rand_wtp <= 0.9, "Medium", "High")),
      Client = ifelse(rand_client <= 0.25, "WUC", ifelse(rand_client <= 0.5, "BPC", ifelse(rand_client <= 0.75, "Letshego", "Absa")))
    ))
  }
}

# Populate Willingness to Pay and Client based on Honoured quartile
for (i in 1:nrow(data2)) {
  honoured_quartile <- data$Honoured_Quartile[i]
  values <- set_values_based_on_quartile(honoured_quartile, runif(1), runif(1))
  data2$Willingness_to_Pay[i] <- values$Willingness_to_Pay
  data2$Client[i] <- values$Client
}

# Optionally print a portion of the data to verify
View(data2)

# Initialize the PTP_This_Month column to 0
data2$PTP_This_Month <- 0

# Loop through each row to determine the value of PTP_This_Month
for (i in 1:nrow(data2)) {
  randnum <- runif(1)
  sum_adjust <- ifelse(data2$Client[i] == "WUC", 0.25, ifelse(data2$Client[i] == "BPC", 0.2, 0))
  randsum <- randnum + sum_adjust
  
  # Determine PTP_This_Month based on Willingness to Pay and adjusted random sum
  if (data2$Willingness_to_Pay[i] == "High" && randsum >= 0.5) {
    data2$PTP_This_Month[i] <- 1
  } else if (data2$Willingness_to_Pay[i] == "Medium" && randsum >= 0.6) {
    data2$PTP_This_Month[i] <- 1
  } else if (data2$Willingness_to_Pay[i] == "Low" && randsum >= 0.7) {
    data2$PTP_This_Month[i] <- 1
  }
}

# Optionally print a portion of the data to verify
View(data2)

# Loop through each row to calculate Installment Due
for (i in 1:nrow(data2)) {
  if (data2$PTP_This_Month[i] == 1) {
    randnum <- runif(1, min = 0.07, max = 0.15)
    adjuster <- ifelse(data2$Willingness_to_Pay[i] == "Low", 0.01,
                       ifelse(data2$Willingness_to_Pay[i] == "Medium", 0.03, 0.05))
    data2$Installment_Due[i] <- randnum * data2$Amount_Owed[i] + adjuster * data2$Amount_Owed[i]
  } else {
    data2$Installment_Due[i] <- 0  # No installment due if no PTP this month
  }
}

# Optionally print a portion of the data to verify
View(data2)

# Initialize the Paid_This_Month and AA_Paid columns to 0
data2$Paid_This_Month <- 0
data2$AA_Paid <- 0

# Loop through each row to determine the value of Paid_This_Month and compute AA_Paid
for (i in 1:nrow(data2)) {
  if (data2$PTP_This_Month[i] == 1) {
    randnum <- runif(1, min = 0.2, max = 0.5)
    
    # Adjust randnum based on Willingness to Pay and Client
    if (data2$Willingness_to_Pay[i] == "High") {
      randnum <- randnum + 0.1
    } else if (data2$Willingness_to_Pay[i] == "Medium") {
      randnum <- randnum + 0.05
    }
    
    if (data2$Client[i] == "WUC") {
      randnum <- randnum + 0.1
    } else if (data2$Client[i] == "BPC") {
      randnum <- randnum + 0.05
    }
    
    # Determine if the person paid this month
    if (randnum > 0.5) {
      data2$Paid_This_Month[i] <- 1
      data2$AA_Paid[i] <- data2$Installment_Due[i] * randnum
    }
  }
}

# Optionally print a portion of the data to verify
View(data2)

# Select only the numeric variables
numeric_data <- data2[, c("Salary", "Honoured", "Unhonoured", "Amount_Owed", "Installment_Due", "AA_Paid")]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")  # Handling any NA values

# Print the correlation matrix
print(cor_matrix)

# Cross-tabulation of Client and Willingness to Pay
table_client_wtp <- table(data2$Client, data2$Willingness_to_Pay)
print(table_client_wtp)

# Chi-square test of independence for Client and Willingness to Pay
chi_square_client_wtp <- chisq.test(table_client_wtp)
print(chi_square_client_wtp)

# Cross-tabulation of Client and PTP_This_Month
table_client_ptp <- table(data2$Client, data2$PTP_This_Month)
print(table_client_ptp)

# Chi-square test of independence for Client and PTP_This_Month
chi_square_client_ptp <- chisq.test(table_client_ptp)
print(chi_square_client_ptp)

# Cross-tabulation of Client and Paid_This_Month
table_client_paid <- table(data2$Client, data2$Paid_This_Month)
print(table_client_paid)

# Chi-square test of independence for Client and Paid_This_Month
chi_square_client_paid <- chisq.test(table_client_paid)
print(chi_square_client_paid)

# Cross-tabulation of Willingness to Pay and Paid_This_Month
table_willingness_paid <- table(data2$Willingness_to_Pay, data2$Paid_This_Month)
print(table_willingness_paid)

# Chi-square test of independence for Willingness to Pay and Paid_This_Month
chi_square_willingness_paid <- chisq.test(table_willingness_paid)
print(chi_square_willingness_paid)


# Load necessary library
library(stats)

# Logistic Regression for PTP This Month
model_ptp_data2 <- glm(PTP_This_Month ~ Salary + Honoured + Unhonoured + Amount_Owed + Willingness_to_Pay + Client,
                       data = data2, family = binomial())
summary(model_ptp_data2)

# Logistic Regression for Paid This Month (All Data)
model_paid_all_data2 <- glm(Paid_This_Month ~ Salary + Honoured + Unhonoured + Amount_Owed + Willingness_to_Pay + Client,
                            data = data2, family = binomial())
summary(model_paid_all_data2)

# Logistic Regression for Paid This Month (Only for Debtors with a PTP)
data_with_ptp_data2 <- subset(data2, PTP_This_Month == 1)
model_paid_ptp_data2 <- glm(Paid_This_Month ~ Salary + Honoured + Unhonoured + Amount_Owed + Willingness_to_Pay + Client,
                            data = data_with_ptp_data2, family = binomial())
summary(model_paid_ptp_data2)

# Specifically rename columns in data2 to ensure exact matches
names(data2)[names(data2) == "Salary"] <- "Salary"
names(data2)[names(data2) == "Honoured"] <- "PTPs_Honoured"
names(data2)[names(data2) == "Unhonoured"] <- "PTPs_Unhonoured"
names(data2)[names(data2) == "Amount_Owed"] <- "Amount_Owed"
names(data2)[names(data2) == "Willingness_to_Pay"] <- "Willingness_to_Pay"
names(data2)[names(data2) == "Client"] <- "Client"
names(data2)[names(data2) == "PTP_This_Month"] <- "PTP_This_Month"
names(data2)[names(data2) == "Installment_Due"] <- "Installment_Due"
names(data2)[names(data2) == "Paid_This_Month"] <- "Paid_This_Month"
names(data2)[names(data2) == "AA_Paid"] <- "AA_Paid"

# Additionally add and rename the Av_Length_of_Calls from data to data2
data2$Av_Length_of_Calls <- data$Av_Length_of_Calls

# Verify the structure and naming
str(data2)

write_csv(data2, "STAT 305 Statistical Computing/STdatasetFinal.csv")

