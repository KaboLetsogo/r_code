library(readr)
library(tidyverse)
library(biotools)
library(MASS)
library(MVN)
library(caret)
library(biotools)
library(car)

data <- read_csv("STAT416/framingham dataset.csv")

str(data)

data$male <- as.factor(data$male)
data$education <- as.factor(data$education)
data$currentSmoker <- as.factor(data$currentSmoker)
data$BPMeds <- as.factor(data$BPMeds)
data$prevalentStroke <- as.factor(data$prevalentStroke)
data$prevalentHyp <- as.factor(data$prevalentHyp)
data$TenYearCHD <- as.factor(data$TenYearCHD)
data$diabetes <- as.factor(data$diabetes)

str(data)




##### QUESTION 1

missing_proportion <- (colSums(is.na(data)) / nrow(data)) * 100
missing_proportion <- round(missing_proportion[missing_proportion > 0],digits=2)
missing_proportion

data <- na.omit(data)


##### QUESTION 2
data_num <- data %>% dplyr::select(glucose,sysBP,diaBP,BMI)#,age,cigsPerDay,heartRate,glucose)
data_num <- data_num %>% mutate(across(everything(), as.numeric))
data_num <- data.frame(data_num)


summary(data_num)
#adjust so that all data is strictly positive for boxCox
#data_num <- data_num %>% mutate(cigsPerDay = cigsPerDay + 0.00000000000000000001)

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
    sample_mean = sample_mean,
    hypothesized_mean = hypothesized_mean,
    inv_cov = inv_cov,
    T2_statistic = t2_stat,
    F_statistic = f_stat,
    F_critical = f_crit,
    decision = decision,
    Simultaneous_CI = simultaneous_ci,
    Bonferroni_CI = bonferroni_ci
  )
}

data_num_temp <- data_num %>% dplyr::select(sysBP, diaBP, BMI, glucose)
sample_mean <- colMeans(data_num_temp, na.rm = TRUE)
sample_mean <- matrix(sample_mean, ncol = 1)  # Convert to column matrix

hypothesized_mean <- matrix(c(120,80,25,100), ncol = 1)

cov_matrix <- cov(data_num_temp, use = "complete.obs")

# Define sample size
n <- nrow(data_num_temp)  # This is the number of non-missing rows after omitting NA values
p <- ncol(data_num_temp)
# Perform one-sample Hotelling's T² test with confidence intervals
result <- one_sample_hotelling(sample_mean, hypothesized_mean, cov_matrix, n)

# Display the results
print(result)


##Simultaneous Confidence Intervals
T.ci <- function(mu, Sigma, n, avec=rep(1,length(mu)), level=0.95){
  p <- length(mu)
  if(nrow(Sigma)!=p) stop("Need length(mu) == nrow(Sigma).")
  if(ncol(Sigma)!=p) stop("Need length(mu) == ncol(Sigma).")
  if(length(avec)!=p) stop("Need length(mu) == length(avec).")
  if(level <=0 | level >= 1) stop("Need 0 < level < 1.")
  zhat <- crossprod(avec, mu)
  if(length(n)==1L){
    cval <- qchisq(0.95, df=p)
    zvar <- crossprod(avec, Sigma %*% avec) / n
  } 
  const <- sqrt(cval * zvar)
  c(lower = zhat - const, upper = zhat + const)
}

xbar = sample_mean
S = cov_matrix

TCI<- bon <- NULL
alpha <- 1 - (0.05/(2*p))
for(k in 1:p){
  avec <- rep(0, p)
  avec[k] <- 1
  TCI <- c(TCI, T.ci(xbar, S, n, avec))
  
  bon <- c(bon,
           xbar[k] - sqrt(S[k,k]/n) * qnorm(alpha),
           xbar[k] + sqrt(S[k,k]/n) * qnorm(alpha))
}

rtab <- rbind(TCI, bon)
round(rtab, 3)
xbar










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





###### QUESTION 5 

data$education <- as.factor(data$education) 
table(data$education)
data_split <- split(data,data$education)

Ybar <- lapply(data_split, function(subset) {
  colMeans(subset[,c("sysBP","diaBP","totChol")])
})

cov_matrixes <- lapply(data_split, function(subset) {
  cov(subset[,c("sysBP","diaBP","totChol")])
})

print(Ybar)
print(cov_matrixes)

nrow(data_split$`1`) + nrow(data_split$`2`) + nrow(data_split$`3`) + nrow(data_split$`4`) 

boxM(data[, c("sysBP","diaBP","totChol")], grouping = data$education)

fit <- lm(cbind(sysBP,diaBP,totChol) ~ education, data = data)
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