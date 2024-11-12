

S=matrix(c(5,2,2,4), nrow=2, ncol=2)
S
solve(S)

mu=matrix(c(11,14),nrow=2)
mu

mu_0=matrix(c(10,12),nrow=2)
n=3
p=2
T2=20*t(mu-mu_0)%*%solve(S)%*%(mu-mu_0)
T2


cv<- (((n-1)*p)/(n-p))*qf(0.95, df1=2, df2=1)
cv

pval <- 1 - pf(20, df1=p, df2=18)
pval

ifelse(T2>cv,"Reject H0","Fail to Reject H0")
#####################################################################

# Load necessary library
library(MASS)

# Given data
p <- 2  # Number of variables
n <- 20  # Sample size
x_bar <- c(11, 14)  # Sample mean vector
S <- matrix(c(5, 2, 2, 4), nrow = 2)  # Sample covariance matrix
mu0 <- c(10, 12)  # Hypothesized population mean

# Number of simulations
num_simulations <- 10000

# Calculate the inverse of the sample covariance matrix
S_inv <- solve(S)

# Storage for T^2 values
T2_values <- numeric(num_simulations)
head(T2_values)

# Simulation loop
set.seed(100)  # For reproducibility
for (i in 1:num_simulations) {
  # Simulate a sample mean vector from the multivariate normal distribution
  simulated_sample <- mvrnorm(n = n, mu = x_bar, Sigma = S)
  simulated_mean <- colMeans(simulated_sample)
  
  # Calculate T^2 statistic
  T2_values[i] <- n * t(simulated_mean - mu0) %*% S_inv %*% (simulated_mean - mu0)
}

# Plot the histogram of T^2 values
hist(T2_values, breaks = 50, probability = TRUE, main = expression(T^2 ~ "Distribution from Simulation"),
     xlab = expression(T^2), col = "lightblue", border = "black")

# Add a theoretical Chi-squared distribution for comparison
curve(dchisq(x, df = p), add = TRUE, col = "red", lwd = 2)
curve(df(x, df1 = p,df2 = n-p), add = TRUE, col = "red", lwd = 2)


# Display results
cat("Mean of T^2:", mean(T2_values), "\n")
cat("Variance of T^2:", var(T2_values), "\n")

# Calculate the critical value for T^2
F_critical <- qf(0.95, df1 = p, df2 = n - p)
T2_critical <- (n - 1) * p / (n - p) * F_critical

# Calculate the proportion of T^2 values that exceed the critical value
proportion_exceed <- mean(T2_values > T2_critical)

# Display the results
cat("The critical value for T^2 at alpha =", 0.05, "is:", T2_critical, "\n")
cat("Proportion of simulated T^2 values that exceed the critical value:", proportion_exceed, "\n")

head(T2_values,10)
###########################################################################################

S=matrix(c(100,15,15,10), nrow=2, ncol=2)
S
solve(S)

mu=matrix(c(210,27),nrow=2)
mu

mu_0=matrix(c(200,25),nrow=2)
n=30
p=2
T2=20*t(mu-mu_0)%*%solve(S)%*%(mu-mu_0)
T2

pval <- 1 - pf(T2, df1=p, df2=n-p)
pval

cv<- (((n-1)*p)/(n-p))*qf(0.95, df1=p, df2=n-p)
cv
ifelse(T2>cv,"Reject H0","Fail to Reject H0")


##########################################################################


