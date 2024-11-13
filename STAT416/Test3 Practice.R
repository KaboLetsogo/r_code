library(readr)
library(tidyverse)
library(biotools)
library(MASS)
library(MVN)
library(caret)
library(biotools)

data <- read_csv("STAT416/UCI Heart Disease Data set.csv")

str(data)

##### QUESTION 1

missing_proportion <- (colSums(is.na(data)) / nrow(data)) * 100
missing_proportion <- round(missing_proportion[missing_proportion > 0],digits=2)
#missing_proportion

data <- na.omit(data)


##### QUESTION 2
data_num <- data %>% dplyr::select(oldpeak, thalch, chol, trestbps, age)
data_num <- data_num %>% mutate(across(everything(), as.numeric))
data_num <- data.frame(data_num)


summary(data_num)
#adjust so that all data is strictly positive for boxCox
data_num <- data_num %>% mutate(oldpeak = oldpeak + 0.00000000000000000001)

#mvn_result <- mvn(data = data_num, mvnTest = "mardia", multivariatePlot = "qq")
#mvn_result

mardia_test <- mvn(data = data_num, mvnTest = "mardia",showOutliers = T, bc = T, showNewData = T)
mardia_test

#bc = T doesnt transform the data , it gives you stuff you need in order to do transformation, you need to
#know formula and two special cases for boxCox transform being 0 and 1

# Setup and apply Box-Cox transformation
preprocessParams <- preProcess(data_num, method = "BoxCox")
data_num_bc <- predict(preprocessParams, data_num)  # Transformed data
data_num <- data_num_bc
mardia_test_bc <- mvn(data = data_num_bc, mvnTest = "mardia",showOutliers = T)
mardia_test_bc


mahalanobis_distances <- mahalanobis(data_num, colMeans(data_num, na.rm = TRUE), cov(data_num, use = "complete.obs"))
threshold <- qchisq(0.95, df = ncol(data_num))  # 95% confidence level
outliers <- which(mahalanobis_distances > threshold)
#outliers

##### QUESTION 3

# Function for one-sample Hotelling's T^2 test with Simultaneous and Bonferroni Confidence Intervals
one_sample_hotelling <- function(sample_mean, hypothesized_mean, cov_matrix, n, alpha = 0.05) {
  # Calculate degrees of freedom
  p <- length(sample_mean)
  df1 <- p
  df2 <- n - p
  
  # Hotelling's T^2 calculation
  diff_mean <- sample_mean - hypothesized_mean
  inv_cov <- solve(cov_matrix)
  t2_stat <- n * t(diff_mean) %*% inv_cov %*% diff_mean
  t2_stat <- as.numeric(t2_stat) # Convert to scalar
  
  # F-statistic conversion
  f_stat <- (df2 / (p * (n - 1))) * t2_stat
  f_crit <- ((p * (n - 1)) / df2) * qf(1 - alpha, df1, df2)
  decision <- ifelse(f_stat > f_crit, "Reject H0", "Fail to reject H0")
  
  # Calculate Simultaneous Confidence Intervals
  t2_crit <- qf(1 - alpha, p, n - p) # F critical value for simultaneous CI
  simultaneous_ci <- list()
  for (j in 1:p) {
    se_simultaneous <- sqrt((t2_crit * cov_matrix[j, j]) / n)
    simultaneous_ci[[j]] <- c(sample_mean[j] - se_simultaneous, sample_mean[j] + se_simultaneous)
  }
  
  # Calculate Bonferroni Confidence Intervals
  bon_crit <- qt(1 - alpha / (2 * p), n - 1) # Bonferroni critical value
  bonferroni_ci <- list()
  for (j in 1:p) {
    se_bon <- sqrt(cov_matrix[j, j] / n)
    bonferroni_ci[[j]] <- c(sample_mean[j] - bon_crit * se_bon, sample_mean[j] + bon_crit * se_bon)
  }
  
  # Compile results into a list
  list(
    T2_statistic = t2_stat,
    F_statistic = f_stat,
    F_critical = f_crit,
    decision = decision,
    Simultaneous_CI = simultaneous_ci,
    Bonferroni_CI = bonferroni_ci
  )
}

data_num_temp <- data_num %>% dplyr::select(chol, trestbps)
sample_mean <- colMeans(data_num_temp, na.rm = TRUE)
sample_mean <- matrix(sample_mean, ncol = 1)  # Convert to column matrix

hypothesized_mean <- matrix(c(200, 120), ncol = 1)

cov_matrix <- cov(data_num_temp, use = "complete.obs")

# Define sample size
n <- nrow(data_num_temp)  # This is the number of non-missing rows after omitting NA values

# Perform one-sample Hotelling's T² test with confidence intervals
result <- one_sample_hotelling(sample_mean, hypothesized_mean, cov_matrix, n)

# Display the results
print(result)

# Define fixed parameters
p <- 2                       # Number of variables
mu0 <- c(200, 120)           # Hypothesized mean vector
S <- matrix(c(3000, 100,     # Covariance matrix
              100, 250), nrow = 2)
num_simulations <- 10000     # Number of simulations per sample size
alpha <- 0.05                # Significance level

# Define range of sample sizes for sensitivity analysis
sample_sizes <- c(20, 30, 50, 100)  

# Initialize storage for results
sensitivity_results <- data.frame(Sample_Size = sample_sizes, Power = NA)

# Loop through each sample size
for (n in sample_sizes) {
  # Calculate inverse of covariance matrix
  S_inv <- solve(S)
  
  # Storage for T^2 values for each sample size
  T2_values <- numeric(num_simulations)
  
  # Simulation loop
  set.seed(100)  # For reproducibility
  for (i in 1:num_simulations) {
    # Simulate a sample mean vector from the multivariate normal distribution with a slight mean shift
    shifted_mean <- mu0 + c(5, 5)  # Small shift in mean for sensitivity analysis
    simulated_sample <- mvrnorm(n = n, mu = shifted_mean, Sigma = S)
    simulated_mean <- colMeans(simulated_sample)
    
    # Calculate T^2 statistic
    T2_values[i] <- n * t(simulated_mean - mu0) %*% S_inv %*% (simulated_mean - mu0)
  }
  
  # Calculate the critical T^2 value for comparison
  F_critical <- qf(1 - alpha, df1 = p, df2 = n - p)
  T2_critical <- (n - 1) * p / (n - p) * F_critical
  
  # Calculate power as the proportion of simulations where T^2 exceeds the critical value
  power <- mean(T2_values > T2_critical)
  
  # Store the result in sensitivity results data frame
  sensitivity_results$Power[sensitivity_results$Sample_Size == n] <- power
}

# Display the sensitivity results
print(sensitivity_results)


##### QUESTION 4

data$sex <- as.factor(data$sex)


# Select relevant variables and the group variable (sex)
data_boxM <- data %>% dplyr::select(chol, trestbps, sex)  # 'chol' and 'trestbps' for analysis, 'sex' is the grouping variable

# Run Box's M Test for equality of covariance matrices
boxM_result <- boxM(data_boxM[, c("chol", "trestbps")], grouping = data_boxM$sex)

# Display the results
print(boxM_result)

# Function for Two-Sample Independent Hotelling's T^2 Test with Confidence Intervals
hotelling_two_sample <- function(group1_data, group2_data, equal_covariances = TRUE, alpha = 0.05) {
  # Sample sizes
  n1 <- nrow(group1_data)
  n2 <- nrow(group2_data)
  
  # Sample means
  mean_group1 <- colMeans(group1_data, na.rm = TRUE)
  mean_group2 <- colMeans(group2_data, na.rm = TRUE)
  
  # Difference in means
  diff_mean <- mean_group1 - mean_group2
  
  # Sample covariance matrices
  cov_group1 <- cov(group1_data, use = "complete.obs")
  cov_group2 <- cov(group2_data, use = "complete.obs")
  
  # Choose the appropriate covariance matrix
  if (equal_covariances) {
    pooled_cov <- ((n1 - 1) * cov_group1 + (n2 - 1) * cov_group2) / (n1 + n2 - 2)
    S_inv <- solve(pooled_cov)
    T2_stat <- (n1 * n2 / (n1 + n2)) * as.numeric(t(diff_mean) %*% S_inv %*% diff_mean)
    cov_matrix <- pooled_cov
  } else {
    S_inv <- solve((cov_group1 / n1) + (cov_group2 / n2))
    T2_stat <- as.numeric(t(diff_mean) %*% S_inv %*% diff_mean)
    cov_matrix <- (cov_group1 / n1) + (cov_group2 / n2)
  }
  
  # Degrees of freedom
  p <- length(mean_group1)
  df1 <- as.numeric(p)
  if (equal_covariances) {
    df2 <- n1 + n2 - p - 1
  } else {
    df2 <- as.numeric(((cov_group1 / n1) + (cov_group2 / n2))^2 /
                        (((cov_group1 / n1)^2 / (n1 - 1)) + ((cov_group2 / n2)^2 / (n2 - 1))))
  }
  
  # F-statistic conversion
  f_stat <- if (equal_covariances) {
    as.numeric((df2 / (p * (n1 + n2 - 2))) * T2_stat)
  } else {
    as.numeric(T2_stat * (df2 - p + 1) / (df2 * p))
  }
  
  # Critical F-value
  f_critical <- as.numeric(qf(1 - alpha, df1, df2))
  
  # Decision
  decision <- ifelse(f_stat > f_critical, "Reject H0", "Fail to Reject H0")
  
  # Simultaneous Confidence Intervals
  t2_critical <- qchisq(1 - alpha, df = p)
  simultaneous_ci <- matrix(ncol = 2, nrow = p)
  rownames(simultaneous_ci) <- names(diff_mean)
  colnames(simultaneous_ci) <- c("Lower", "Upper")
  
  for (j in 1:p) {
    se_simultaneous <- sqrt(t2_critical * cov_matrix[j, j] / (n1 + n2))
    simultaneous_ci[j, ] <- c(diff_mean[j] - se_simultaneous, diff_mean[j] + se_simultaneous)
  }
  
  # Bonferroni Confidence Intervals
  alpha_bon <- alpha / (2 * p)
  bonferroni_ci <- matrix(ncol = 2, nrow = p)
  rownames(bonferroni_ci) <- names(diff_mean)
  colnames(bonferroni_ci) <- c("Lower", "Upper")
  
  for (j in 1:p) {
    se_bon <- sqrt(cov_matrix[j, j] / (n1 + n2))
    critical_bon <- qt(1 - alpha_bon, df = min(n1, n2) - 1)
    bonferroni_ci[j, ] <- c(diff_mean[j] - critical_bon * se_bon, diff_mean[j] + critical_bon * se_bon)
  }
  
  # Output results
  list(
    T2_statistic = T2_stat,
    F_statistic = f_stat,
    F_critical = f_critical,
    decision = decision,
    Simultaneous_CI = simultaneous_ci,
    Bonferroni_CI = bonferroni_ci
  )
}

# Example usage with equal_covariances set to TRUE or FALSE
# Assuming 'male_data' and 'female_data' are subsets of your data for the two groups
male_data <- data[data$sex == "Male", c("chol", "trestbps")]
female_data <- data[data$sex == "Female", c("chol", "trestbps")]
result <- hotelling_two_sample(male_data, female_data, equal_covariances = FALSE, alpha = 0.01)
print(result)
