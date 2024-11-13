library(readr)
library(tidyverse)
library(biotools)
library(MASS)
library(MVN)
library(caret)
library(biotools)
library(car)

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

data_bc <- data  
data_bc[, colnames(data_num_bc)] <- data_num_bc

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

data_num_temp <- data_num_bc %>% dplyr::select(chol, trestbps)
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

data_bc$sex <- as.factor(data_bc$sex)


# Select relevant variables and the group variable (sex)
data_boxM <- data_bc %>% dplyr::select(chol, trestbps, sex)  # 'chol' and 'trestbps' for analysis, 'sex' is the grouping variable

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
male_data <- data_bc[data_bc$sex == "Male", c("chol", "trestbps")]
female_data <- data_bc[data_bc$sex == "Female", c("chol", "trestbps")]
result <- hotelling_two_sample(male_data, female_data, equal_covariances = FALSE, alpha = 0.01)
print(result)

# Function to perform sensitivity analysis using Hotelling's T^2 test
sensitivity_analysis_hotelling <- function(base_data1, base_data2, n_simulations = 10000, alpha = 0.05) {
  
  # Baseline statistics
  n1 <- nrow(base_data1)
  n2 <- nrow(base_data2)
  
  base_mean1 <- colMeans(base_data1)
  base_mean2 <- colMeans(base_data2)
  
  cov1 <- cov(base_data1)
  cov2 <- cov(base_data2)
  
  # Define variability range for sensitivity (e.g., 10% deviation)
  mean_variability <- 0.1
  cov_variability <- 0.1
  
  # Store simulation results
  results <- data.frame(T2_statistic = numeric(n_simulations), F_statistic = numeric(n_simulations),
                        F_critical = numeric(n_simulations), decision = character(n_simulations),
                        stringsAsFactors = FALSE)
  
  for (i in 1:n_simulations) {
    
    # Vary means by adding random noise
    varied_mean1 <- base_mean1 + rnorm(length(base_mean1), mean = 0, sd = mean_variability * base_mean1)
    varied_mean2 <- base_mean2 + rnorm(length(base_mean2), mean = 0, sd = mean_variability * base_mean2)
    
    # Vary covariances by adding random noise to each element
    varied_cov1 <- cov1 + matrix(rnorm(length(cov1), mean = 0, sd = cov_variability * cov1), ncol = ncol(cov1))
    varied_cov2 <- cov2 + matrix(rnorm(length(cov2), mean = 0, sd = cov_variability * cov2), ncol = ncol(cov2))
    
    # Ensure covariance matrices are symmetric
    varied_cov1 <- (varied_cov1 + t(varied_cov1)) / 2
    varied_cov2 <- (varied_cov2 + t(varied_cov2)) / 2
    
    # Perform Hotelling's T^2 test with the varied parameters
    test_result <- hotelling_two_sample(
      group1_data = mvrnorm(n = n1, mu = varied_mean1, Sigma = varied_cov1),
      group2_data = mvrnorm(n = n2, mu = varied_mean2, Sigma = varied_cov2),
      equal_covariances = FALSE,
      alpha = alpha
    )
    
    # Store results for each simulation
    results$T2_statistic[i] <- test_result$T2_statistic
    results$F_statistic[i] <- test_result$F_statistic
    results$F_critical[i] <- test_result$F_critical
    results$decision[i] <- test_result$decision
  }
  
  # Analyze results
  decision_summary <- table(results$decision)
  list(
    results = results,
    decision_summary = decision_summary,
    rejection_rate = mean(results$decision == "Reject H0")
  )
}

# Usage example assuming groups defined up there

sensitivity_results <- sensitivity_analysis_hotelling(male_data, female_data, n_simulations = 100, alpha = 0.01)
print(sensitivity_results$decision_summary)
print(sensitivity_results$rejection_rate)


###### QUESTION 5 

data$thal <- as.factor(data$thal) 
table(data$thal)
data_split <- split(data,data$thal)

Ybar <- lapply(data_split, function(subset) {
  colMeans(subset[,c("trestbps","chol","oldpeak")])
})

cov_matrixes <- lapply(data_split, function(subset) {
  cov(subset[,c("trestbps","chol","oldpeak")])
})

print(Ybar)
print(cov_matrixes)

boxM(data[, c("trestbps","chol","oldpeak")], grouping = data$thal)

filtered_data <-  data[data$thal != "",]
table(filtered_data$thal)

fit <- lm(cbind(trestbps,chol,oldpeak) ~ thal, data = filtered_data)
manova_model=Manova(fit, type = "II")

summary(manova_model)

# Group sample sizes
group_sizes <- sapply(data_split, nrow)

# Calculate pooled covariance matrix (W)
pooled_cov_matrix <- Reduce(`+`, lapply(1:length(data_split), function(i) {
  (group_sizes[i] - 1) * cov_matrixes[[i]]
})) / (sum(group_sizes) - length(group_sizes))

# Enhanced function to calculate Bonferroni confidence intervals with mean differences
calculate_bonferroni_ci_with_diff <- function(Ybar, pooled_cov, group_sizes, alpha = 0.05) {
  g <- length(Ybar)  # number of groups
  p <- length(Ybar[[1]])  # number of variables
  
  # Initialize list to store confidence intervals and mean differences
  bonferroni_ci <- list()
  
  # Loop over all pairs of groups (k, l) with k < l
  for (k in 1:(g - 1)) {
    for (l in (k + 1):g) {
      # Calculate mean difference for each variable between groups k and l
      mean_diff <- Ybar[[k]] - Ybar[[l]]
      
      # Matrix to store mean differences and confidence intervals for each variable
      ci_matrix <- matrix(ncol = 3, nrow = p)
      rownames(ci_matrix) <- names(Ybar[[k]])
      colnames(ci_matrix) <- c("Mean_Diff", "Lower", "Upper")
      
      # Bonferroni adjusted alpha level
      bon_alpha <- alpha / (p * g * (g - 1))
      t_critical <- qt(1 - bon_alpha, df = sum(group_sizes) - g)
      
      # Calculate confidence interval for each variable
      for (j in 1:p) {
        # Standard error calculation
        se <- sqrt((1 / group_sizes[k] + 1 / group_sizes[l]) * pooled_cov[j, j])
        
        # Compute margin
        margin <- t_critical * se
        ci_matrix[j, ] <- c(mean_diff[j], mean_diff[j] - margin, mean_diff[j] + margin)
      }
      
      # Store in list with labels for the group pair
      bonferroni_ci[[paste0("Group_", names(Ybar)[k], "_vs_Group_", names(Ybar)[l])]] <- ci_matrix
    }
  }
  return(bonferroni_ci)
}

# Calculate Bonferroni confidence intervals with mean differences
bonferroni_intervals_with_diff <- calculate_bonferroni_ci_with_diff(Ybar, pooled_cov_matrix, group_sizes, alpha = 0.05)
print(bonferroni_intervals_with_diff)

# Ensure categorical variables are treated as factors
data$sex <- as.factor(data$sex)
data$slope <- as.factor(data$slope)


######## Question 7

# Fit the multivariate regression model
multivariate_model <- lm(cbind(chol, oldpeak, trestbps) ~ age + sex + thalch + slope, data = data)

# Summarize the model to see individual regression results
summary(multivariate_model)

# Assumption Check 1: Mardia's Test for Multivariate Normality of Residuals
residuals_matrix <- residuals(multivariate_model)
mardia_test <- mvn(data = residuals_matrix, mvnTest = "mardia")
print(mardia_test)

# Assumption Check 2: Box's M Test for Equality of Covariance Matrices (e.g., by sex)
boxM_test <- boxM(data[, c("chol", "oldpeak", "trestbps")], grouping = data$sex)
print(boxM_test)

# Assumption Check 3: Durbin-Watson Test for Independence of Residuals
# Apply Durbin-Watson test on each individual outcome regression
dw_test_serum <- durbinWatsonTest(lm(chol ~ age + sex + thalch   + slope, data = data))
dw_test_oldpeak <- durbinWatsonTest(lm(oldpeak ~ age + sex + thalch   + slope, data = data))
dw_test_bp <- durbinWatsonTest(lm(trestbps ~ age + sex + thalch   + slope, data = data))

# Print Durbin-Watson test results
print(dw_test_serum)
print(dw_test_oldpeak)
print(dw_test_bp)

#If the p-value is less than the significance level (α, often set at 0.05), you reject the null hypothesis (H₀)

#One Sample T2 test
# H0: The sample mean vector is equal to the hypothesized mean vector (Ȳ = μ0)
# H1: The sample mean vector is not equal to the hypothesized mean vector (Ȳ ≠ μ0)

# T² = n * (Ȳ - μ0)^T * S^(-1) * (Ȳ - μ0) use x instead of y




















#Mahalanobis test
# H0: The observation is not an outlier (within critical Mahalanobis distance range)
# H1: The observation is an outlier (exceeds critical Mahalanobis distance range)

# D² = (x - μ)^T * Σ^(-1) * (x - μ)

####### Mardia's test for multivariate normality
# H0: Data follows a multivariate normal distribution
# H1: Data does not follow a multivariate normal distribution

# Calculation of pooled covariance matrix (S_pooled):
# S_pooled = 1 / Σℓ(nℓ - 1) * Σℓ[(nℓ - 1) * Sℓ]

# Calculation of Box's M statistic:
# M = Σℓ(nℓ - 1) * ln|S_pooled| - Σℓ(nℓ - 1) * ln|Sℓ|

# Calculation of adjusted C-statistic:
# u = 1 - (2 * p^2 + 3 * p - 1) / [6 * (p + 1) * (g - 1)]
# C = (1 - u) * M

# Degrees of freedom for C:
# ν = 0.5 * p * (p + 1) * (g - 1)






# Hotelling's T^2 test for comparing two population means with homogeneous covariance matrices

# Hypotheses
# H0: µ1 - µ2 = δ0
# Ha: µ1 - µ2 ≠ δ0

# Test Statistic (Homogeneous Covariance)
# T^2 = (X̄1 - X̄2 - δ0)^T * [(1/n1 + 1/n2) * Spooled]^(-1) * (X̄1 - X̄2 - δ0)
# where Spooled = ((n1 - 1) * S1 + (n2 - 1) * S2) / (n1 + n2 - 2)

# Rejection Region
# Reject H0 if T^2 > c2, where c2 = ((n1 + n2 - 2) * p) / (n1 + n2 - p - 1) * F_p,n1+n2-p-1(α)

# Hotelling's T^2 test for comparing two population means with heterogeneous covariance matrices

# Hypotheses
# H0: µ1 - µ2 = 0
# Ha: µ1 - µ2 ≠ 0

# Test Statistic (Heterogeneous Covariance)
# T^2 = (X̄1 - X̄2)^T * [(1/n1) * S1 + (1/n2) * S2]^(-1) * (X̄1 - X̄2)

# Rejection Region
# Reject H0 if T^2 > c2, where c2 = (p * v) / (v - p + 1) * F_p, v-p+1(α)
# v is calculated using the trace formula provided above

# Example calculation
# Spooled <- ((n1 - 1) * S1 + (n2 - 1) * S2) / (n1 + n2 - 2) for homogeneous case
# v <- complex formula using trace functions for heterogeneous case





# MANOVA Hypotheses
# H0: τ1 = τ2 = ... = τg = 0  (No treatment effect)
# H1: At least one τℓ ≠ 0     (Treatment effect exists)

# Define Within-Group Matrix W
# W = (n1 - 1) * S1 + (n2 - 1) * S2 + ... + (ng - 1) * Sg
# where Sℓ is the covariance matrix of the ℓ-th group
# df_W = ∑(nℓ - 1) = ∑ nℓ - g

# Define Between-Group Matrix B
# B = ∑(nℓ * (X̄ℓ - X̄)(X̄ℓ - X̄)^T)
# where X̄ℓ is the mean vector for the ℓ-th group and X̄ is the overall mean
# df_B = g - 1

# Wilks' Lambda Calculation
# Lambda = |W| / |B + W|
# |#W| = Determinant of the Within-Group SSP Matrix
# |#B + W| = Determinant of Total SSP Matrix (B + W)

# Example calculation for Lambda:
# Lambda = (10 * 24 - 1^2) / (88 * 72 - (-11)^2) = 239 / 6215 = 0.0385

# Hypotheses for Wilks' Lambda
# H0: Lambda = 1 (No difference among groups)
# H1: Lambda < 1 (Difference exists among groups)

# Decision Rule:
# Reject H0 if Lambda < critical value or p-value < α

















# Bonferroni Confidence Intervals for Treatment Effects

# When the null hypothesis is rejected, we construct simultaneous confidence intervals 
# for treatment effects using the Bonferroni approach. The (1 - α)100% confidence intervals 
# for the components of the true differences τ_kj - τ_ℓj, for all j = 1, 2, ..., p and ℓ < k = 1, 2, ..., g 
# is given by:

#  X̄_kj - X̄_ℓj ± t_(n - g, α / (p g(g - 1))) * sqrt((1/n_k + 1/n_ℓ) * (w_jj / (N - g)))

# where:
# - X̄_kj and X̄_ℓj are the sample means for the j-th variable in groups k and ℓ, respectively.
# - t_(n - g, α / (p g(g - 1))) is the critical value from the t-distribution with (n - g) degrees of freedom,
#   adjusted by the Bonferroni correction.
# - n_k and n_ℓ are the sample sizes for groups k and ℓ.
# - w_jj is the j-th diagonal element of the within-group covariance matrix W.
# - N = Σ n_ℓ, the total sample size.
# - g is the number of groups.
# - p is the number of dependent variables being compared.
