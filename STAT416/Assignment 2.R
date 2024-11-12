# Load necessary libraries
library(car)
library(tidyverse)
library(biotools)
library(GGally)
library(MASS)
library(ellipse)
############    QUESTION 4


data("iris")
iris_data <- iris[, 1:4]

# Graphical method: Chi-Square Q-Q plot
mvn_result <- mvn(data = iris_data, mvnTest = "mardia", multivariatePlot = "qq")

# Statistical Tests: Mardia’s and Henze-Zirkler’s test
mardia_test <- mvn(data = iris_data, mvnTest = "mardia")
henze_zirkler_test <- mvn(data = iris_data, mvnTest = "hz")

# Print the results
print(mardia_test)
print(henze_zirkler_test)

# Perform Box's M test to assess homogeneity of covariance matrices across species
box_m_test <- boxM(iris_data, iris$Species)

# Print the result
print(box_m_test)


# Load dataset
data("mtcars")

# Calculate pairwise correlations
cor_matrix <- cor(mtcars)
print(cor_matrix)

# Create a scatterplot matrix for the first 4 columns
ggpairs(mtcars[, 1:6])


# Fit a model to calculate VIFs
model <- lm(mpg ~ ., data = mtcars)
vif_values <- vif(model)
print(vif_values)


# Calculate Mahalanobis distance
mahalanobis_distances <- mahalanobis(mtcars, colMeans(mtcars), cov(mtcars))

# Set the threshold based on chi-square distribution
threshold <- qchisq(0.95, df = ncol(mtcars))

# Identify outliers
outliers <- which(mahalanobis_distances > threshold)
print(outliers)



############    QUESTION 5


mu <- c(3, 5)
Sigma <- matrix(c(2, 1, 1, 3), nrow = 2)

set.seed(123)

n <- 1000
simulated_data <- mvrnorm(n = n, mu = mu, Sigma = Sigma)

simulated_data_df <- as.data.frame(simulated_data)
colnames(simulated_data_df) <- c("X1", "X2")

# Plot the simulated data
plot(simulated_data_df$X1, simulated_data_df$X2,
     main = "Simulated Bivariate Normal Data",
     xlab = "X1", ylab = "X2",
     col = "blue", pch = 16)

# Calculate the sample mean vector
sample_mean <- colMeans(simulated_data_df)

# Calculate the sample covariance matrix
sample_covariance <- cov(simulated_data_df)

# Display results
cat("Sample Mean Vector:\n", sample_mean, "\n")
cat("Sample Covariance Matrix:\n")
print(sample_covariance)

# Compare to the population parameters
cat("Population Mean Vector:\n", mu, "\n")
cat("Population Covariance Matrix:\n")
print(Sigma)

# Define population mean vector and covariance matrix
mu <- c(3, 5)
Sigma <- matrix(c(2, 1, 1, 3), nrow = 2)

# Define the sample sizes to test
sample_sizes <- c(20, 100, 500, 1000)

# Initialize lists to store the results
mean_estimates <- list()
covariance_estimates <- list()

# Loop over each sample size
for (n in sample_sizes) {
  # Storage for mean and covariance estimates across 10 simulations
  means <- matrix(0, nrow = 2, ncol = 10)
  covariances <- array(0, dim = c(2, 2, 10))
  
  # Repeat the simulation 10 times for each sample size
  for (i in 1:10) {
    # Simulate data
    simulated_data <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
    
    # Calculate sample mean and covariance for this simulation
    means[, i] <- colMeans(simulated_data)
    covariances[, , i] <- cov(simulated_data)
  }
  
  # Calculate average sample mean and covariance across the 10 simulations
  avg_mean <- rowMeans(means)
  avg_covariance <- apply(covariances, c(1, 2), mean)
  
  # Store results for each sample size
  mean_estimates[[as.character(n)]] <- avg_mean
  covariance_estimates[[as.character(n)]] <- avg_covariance
  
  # Display results for each sample size
  cat("Sample Size:", n, "\n")
  cat("Average Sample Mean Vector:\n", avg_mean, "\n")
  cat("Average Sample Covariance Matrix:\n")
  print(avg_covariance)
  cat("\n\n")
}


############    QUESTION 6


# Define population mean vector and covariance matrix
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)

# Simulation parameters
n <- 30                  # Sample size
num_simulations <- 10000 # Number of simulations

# Hypothesized mean vector
mu0 <- c(0, 0)

# Storage for T^2 values
T2_values <- numeric(num_simulations)

# Set seed for reproducibility
set.seed(123)

# Calculate T^2 statistic for each simulated sample
for (i in 1:num_simulations) {
  # Simulate a sample of size n
  sample_data <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
  
  # Calculate sample mean vector and sample covariance matrix
  sample_mean <- colMeans(sample_data)
  sample_covariance <- cov(sample_data)
  
  # Calculate Hotelling's T^2 statistic
  T2_values[i] <- n * t(sample_mean - mu0) %*% solve(sample_covariance) %*% (sample_mean - mu0)
}

# Display the first few T^2 values as a check
head(T2_values)

# Plot the histogram of the simulated T^2 values
hist(T2_values, breaks = 50, probability = TRUE, main = expression("Histogram of Simulated " * T^2 * " Values"),
     xlab = expression(T^2), col = "lightblue", border = "black")

# Overlay the theoretical chi-squared distribution with 2 degrees of freedom
curve(dchisq(x, df = 2), from = 0, to = max(T2_values), add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Simulated T^2", "Chi-Squared df=2"), col = c("lightblue", "red"),
       lty = c(1, 1), lwd = c(2, 2))

# Calculate the critical value for T^2 at alpha = 0.05
alpha <- 0.05
p <- 2  # Number of variables
critical_value <- qchisq(1 - alpha, df = p)

# Calculate the proportion of T^2 values that exceed the critical value
empirical_type1_error <- mean(T2_values > critical_value)

# Display results
cat("Critical Value at alpha = 0.05:", critical_value, "\n")
cat("Empirical Type I Error Rate:", empirical_type1_error, "\n")
cat("Expected Type I Error Rate:", alpha, "\n")


############    QUESTION 7


# Given data
n <- 15  # Sample size
x_bar <- c(4.8, 6.2)  # Sample mean vector
S <- matrix(c(1.5, 0.8, 0.8, 2.3), nrow = 2)  # Sample covariance matrix
mu0 <- c(5, 6)  # Hypothesized mean vector

# Number of variables (p)
p <- length(x_bar)

# Hotelling's T^2 statistic
T2 <- n * t(x_bar - mu0) %*% solve(S) %*% (x_bar - mu0)

# Critical value from the F-distribution
alpha <- 0.05
F_critical <- qf(1 - alpha, df1 = p, df2 = n - p)
T2_critical <- ((n - 1) * p / (n - p)) * F_critical

# Decision
decision <- ifelse(T2 > T2_critical, "Reject H0", "Fail to Reject H0")

# Display results
cat("Hotelling's T^2 Statistic:", T2, "\n")
cat("Critical Value for T^2 at alpha = 0.05:", T2_critical, "\n")
cat("Decision:", decision, "\n")


# Calculate the center and radius for the 95% confidence ellipse
confidence_level <- 0.95
F_critical_ellipse <- qf(confidence_level, df1 = p, df2 = n - p)
radius <- sqrt((p * (n - 1) / (n - p)) * F_critical_ellipse)

# Plot the confidence ellipse without explicit scaling
plot(ellipse(S, centre = x_bar, level = confidence_level), type = "l",
     xlab = "X1", ylab = "X2", main = "95% Confidence Ellipse for Mean Vector")
points(x_bar[1], x_bar[2], col = "blue", pch = 19)  # Sample mean
points(mu0[1], mu0[2], col = "red", pch = 4)  # Hypothesized mean
legend("topright", legend = c("Sample Mean", "Hypothesized Mean"),
       col = c("blue", "red"), pch = c(19, 4))


