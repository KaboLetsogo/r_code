# 1. Define the mean vector and covariance matrix
mu <- c(1, 2)  # Mean vector (2-dimensional)
Sigma <- matrix(c(1, 0.5, 0.5, 2), nrow = 2)  # Covariance matrix

# 2. Compute the Cholesky decomposition of the covariance matrix
L <- chol(Sigma)  # Cholesky decomposition returns the upper triangular matrix

# Since we want the lower triangular matrix, we need to transpose it
L <- t(L)

# 3. Generate a random vector Z from the standard normal distribution
set.seed(100)  # Set seed for reproducibility
Z <- rnorm(2)  # Generate a 2-dimensional vector Z ~ N(0, I)

# 4. Apply the transformation X = mu + LZ
X <- mu + L %*% Z

# Display the result
cat("Random sample X from the multivariate normal distribution: \n", X)


# 1. Define the 10x1 mean vector
mu <- rep(0, 10)  # Mean vector with 10 zeros (you can change these values as needed)
mu
# 2. Define a 10x10 covariance matrix (for simplicity, assume an identity matrix first)
Sigma <- diag(10)  # 10x10 identity matrix as the covariance matrix (you can modify it)
Sigma
# Alternatively, define a custom covariance matrix (an example)
#Sigma <- matrix(c(...), nrow = 10, ncol = 10)

# 3. Compute the Cholesky decomposition of the covariance matrix
L <- chol(Sigma)  # Upper triangular matrix
L <- t(L)  # Convert to lower triangular
L
# 4. Generate 10 random vectors Z from the standard normal distribution
set.seed(100)  # Set seed for reproducibility
Z <- matrix(rnorm(10), nrow = 10, ncol = 1)  # 10x1 vector of standard normal random variables
Z
# 5. Apply the transformation X = mu + LZ
X <- mu + L %*% Z
X
# Display the generated 10-dimensional vector
cat("Random sample X from the 10-dimensional multivariate normal distribution: \n", X)


# Define number of samples
n_samples <- 100

# Initialize a matrix to store the samples
samples <- matrix(NA, n_samples, 10)# This returns a matrix of size nXp=100X10

# Generate multiple samples
for (i in 1:n_samples) {
  Z <- rnorm(10)  # Generate a new random Z vector for each sample
  samples[i, ] <- mu + L %*% Z
}
#Z
# View the first few samples
head(samples)
samples
###THE END##############

##############################
n_samples <- 20
mu <- c(0,1,2)
cov_matrix <- matrix(c(1,0.5,0,
                       0.5,3,1,
                       0,1,4), 
                     nrow = 3, byrow = TRUE)

L <- t(chol(cov_matrix))
L

Z <- mvrnorm(mu = mu, Sigma = cov_matrix,n=n_samples)
Z
samples <- mu + Z %*% L
samples
##############################

#########################################Application Example

#since the Cholesky Decomposition preserves the covariance structure, 
#this allows us to simulate realistic portfolio returns, which can be used for 
#risk management and optimization tasks.

#The simulated samples from the multivariate normal distribution, and the 
#portfolio returns generated from them, provide several key insights that are
#valuable for portfolio management and risk assessment. 

# Understanding Distribution of Portfolio Returns.
#Expected Portfolio Return.
#Portfolio Volatility (Risk).
#Value-at-Risk (VaR).
#Effect of Correlations Between Assets.
#Optimization of Weights.
#Stress Testing and Scenario Analysis.


# Load necessary library
library(MASS)

# Step 1: Define mean vector and covariance matrix
mu <- c(0.05, 0.02, 0.03)

cov_matrix <- matrix(c(0.01, 0.004, 0.0035,
                       0.004, 0.0025, 0.0014,
                       0.0035, 0.0014, 0.0049), 
                     nrow = 3, byrow = TRUE)

# Step 2: Perform Cholesky decomposition of the covariance matrix
L <- chol(cov_matrix)

# Step 3: Generate 10,000 independent standard normal samples
n_samples <- 10000
Z <- matrix(rnorm(n_samples * 3), nrow = n_samples, ncol = 3)
head(Z,20)

# Step 4: Transform to multivariate normal samples
samples <- Z %*% t(L) + matrix(rep(mu, each = n_samples), nrow = n_samples, byrow = TRUE)

# Now 'samples' contains 10,000 samples from the multivariate normal distribution
head(samples,20)

# Step 5: Define portfolio weights (equal weights)
weights <- c(1/3, 1/3, 1/3)

# Step 6: Compute portfolio returns for each sample
portfolio_returns <- samples %*% weights

# Step 7: Visualize the distribution of portfolio returns
hist(portfolio_returns, breaks = 50, col = 'lightblue', border = 'black', 
     main = "Simulated Portfolio Returns using Cholesky Decomposition",
     xlab = "Return", ylab = "Frequency")

# Add a vertical line for the median to show how it compares to the mean
abline(v = median(portfolio_returns), col = "red", lwd = 2, lty = 2)  # Median line
abline(v = mean(portfolio_returns), col = "blue", lwd = 2, lty = 1)  # Mean line
legend("topright", legend = c("Mean", "Median"), col = c("blue", "red"),
       lty = c(1, 2), lwd = 2)




#Interpretations:
#A symmetric distribution implies that the probabilities of gains and losses are balanced.
#Balanced Risk and Reward, Lower Extreme Risk.
#################################################################Example 2


# Simulate 10,000 returns from a log-normal distribution
n <- 10000
mu <- 0    # Mean of the normal distribution
sigma <- 0.5  # Standard deviation of the normal distribution
returns <- rlnorm(n, meanlog = mu, sdlog = sigma)

# Plot the histogram of returns
hist(returns, breaks = 50, col = "skyblue", border = "white",
     main = "Simulated Positively Skewed Returns",
     xlab = "Return", freq = TRUE)

# Add a vertical line for the median to show how it compares to the mean
abline(v = median(returns), col = "red", lwd = 2, lty = 2)  # Median line
abline(v = mean(returns), col = "blue", lwd = 2, lty = 1)  # Mean line
legend("topright", legend = c("Mean", "Median"), col = c("blue", "red"),
       lty = c(1, 2), lwd = 2)

############################################Using mvrnorm() to generate samples.

# Mean returns for two assets
mean_vector <- c(0.05, 0.10)

# Covariance matrix (2x2)
cov_matrix <- matrix(c(0.001, 0.0005, 
                       0.0005, 0.002), 
                     nrow = 2)

# Display the covariance matrix
cov_matrix

# Number of samples to generate
n_samples <- 10000

# Generate the samples using mvrnorm()
samples <- mvrnorm(n = n_samples, mu = mean_vector, Sigma = cov_matrix)

# Convert to a data frame for easier manipulation
samples_df <- as.data.frame(samples)

# Rename the columns for clarity
colnames(samples_df) <- c("Asset1_Return", "Asset2_Return")

# Display the first few samples
head(samples_df)
