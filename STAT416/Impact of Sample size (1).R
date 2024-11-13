# Set seed for reproducibility
set.seed(100)

###Example: Covariance Estimation Example: Singular Covariance

# Step 1: Create highly correlated data
n_small <- 3  # Small sample size
n_large <- 100  # Larger sample size

p <- 5  # Number of variables

# Generate two highly correlated variables
x1_small <- rnorm(n_small, mean = 6, sd = 5)
x2_small <-  rnorm(n_small, mean = 5, sd = 10)  # Almost perfect correlation
x3_small <- rnorm(n_small, mean = 0, sd = 0.5)
x4_small <-  rnorm(n_small, mean = 0, sd = 0.01)  # Almost perfect correlation
x5_small <- rnorm(n_small, mean = 1, sd = 1)




# Combine into a data frame (small sample)
data_small <- data.frame(x1_small, x2_small,x3_small, x4_small, x5_small)

# Calculate the covariance matrix for small sample
cov_small <- cov(data_small)
cov_small

# Step 2: Check if the covariance matrix is singular by trying to invert it
tryCatch({
  cov_small_inv <- solve(cov_small)  # Attempt to invert the covariance matrix
  print("Covariance matrix is invertible (not singular).")
}, error = function(e) {
  print("Covariance matrix is singular!")
})



# Step 3: Increase the sample size to correct the situation
# Generate two highly correlated variables
x1_large <- rnorm(n_large, mean = 6, sd = 5)
x2_large <-  rnorm(n_large, mean = 5, sd = 10)  # Almost perfect correlation
x3_large <- rnorm(n_large, mean = 0, sd = 0.5)
x4_large <-  rnorm(n_large, mean = 0, sd = 0.01)  # Almost perfect correlation
x5_large <- rnorm(n_large, mean = 1, sd = 1)

x1_large[1]=x1_large[1]+1

# Combine into a data frame (large sample)
data_large <- data.frame(x1_large, x2_large, x3_large, x4_large, x5_large)

# Calculate the covariance matrix for large sample
cov_large <- cov(data_large)
cov_large

# Check if the covariance matrix is singular for the larger sample
tryCatch({
  cov_large_inv <- solve(cov_large)  # Attempt to invert the covariance matrix
  print("Covariance matrix for large sample is invertible (not singular).")
}, error = function(e) {
  print("Covariance matrix for large sample is singular!")
})

###Example: Covariance Estimation Example: Unstable Covariance

# Set seed for reproducibility
set.seed(100)

# Step 1: Create highly correlated data with a small sample size
n_small <- 5  # Small sample size
p <- 3  # Number of variables

# Generate 3 highly correlated variables (nearly collinear)
x1 <- rnorm(n_small, mean = 0, sd = 1)
x2 <- x1 + rnorm(n_small, mean = 0, sd = 0.01)  # Almost identical to x1
x3 <- x1 + rnorm(n_small, mean = 0, sd = 0.02)  # Almost identical to x1

# Combine into a data frame
data_small <- data.frame(x1, x2, x3)

# Step 2: Calculate the covariance matrix for the small sample
cov_small <- cov(data_small)

# Display the covariance matrix
print("Covariance matrix for small sample (potentially unstable):")
print(cov_small)

# Step 3: Introduce a small change to the data to see the impact on the covariance matrix
x2[1] <- x2[1] + 0.05  # Slight modification in the data

# Recalculate the covariance matrix after the modification
data_small_mod <- data.frame(x1, x2, x3)
cov_small_mod <- cov(data_small_mod)

# Display the modified covariance matrix
print("Covariance matrix after small data change (showing instability):")
print(cov_small_mod)

# Compare the two covariance matrices
print("Difference between original and modified covariance matrices:")
print(cov_small_mod - cov_small)


###############################################################################################
##Impact on Confidence Regions
# Load required libraries
library(MASS)  # For multivariate data generation
library(ellipse)  # For plotting confidence ellipses

# Set seed for reproducibility
set.seed(100)

# Step 1: Generate multivariate normal data
n_small <- 10  # Small sample size
n_large <- 100  # Large sample size
mu <- c(5, 10)  # Mean vector
Sigma <- matrix(c(1, 0.8, 0.8, 1), ncol = 2)  # Covariance matrix with correlation

# Generate data for small and large samples
data_small <- mvrnorm(n_small, mu = mu, Sigma = Sigma)
data_large <- mvrnorm(n_large, mu = mu, Sigma = Sigma)

# Step 2: Calculate sample means and covariance matrices
mean_small <- colMeans(data_small)
mean_small
cov_small <- cov(data_small)
cov_small

mean_large <- colMeans(data_large)
mean_large
cov_large <- cov(data_large)
cov_large

# Step 3: Plot confidence ellipses for both sample sizes
par(mfrow = c(1, 2))  # Set up side-by-side plotting

# Small sample confidence ellipse
plot(data_small, xlab = "X1", ylab = "X2", main = "Small Sample Confidence Region")
lines(ellipse(cov_small, centre = mean_small, level = 0.95), col = "red", lwd = 2)
points(mean_small[1], mean_small[2], col = "red", pch = 19)

# Large sample confidence ellipse
plot(data_large, xlab = "X1", ylab = "X2", main = "Large Sample Confidence Region")
lines(ellipse(cov_large, centre = mean_large, level = 0.95), col = "blue", lwd = 2)
points(mean_large[1], mean_large[2], col = "blue", pch = 19)

################################################################################################
###Impact on Hypothesis Tests

# Load Hotelling's T2 test library
library(Hotelling)

# Set seed for reproducibility
set.seed(123)

# Step 1: Generate multivariate normal data with known population mean
n_small <- 10  # Small sample size
n_large <- 100  # Large sample size
mu <- c(5, 10,6,15)  # Population mean vector
Sigma <- matrix(c(1,0.5,0,0.8,0.5,3,0.4,0,0,0.4,5,0.6,0.8,0,0.6,4), ncol = 4)  # Population covariance matrix

# Generate data for small and large samples
data_small <- as.matrix(mvrnorm(n_small, mu = mu, Sigma = Sigma))
data_large <- as.matrix(mvrnorm(n_large, mu = mu, Sigma = Sigma))

# Step 2: Perform Hotelling's T2 test for small sample
# Hypothesis: H0: mean vector = (5, 10) vs. H1: mean vector != (5, 10)
T2_small <- hotelling.test(data_small, t(as.matrix(mu)))
T2_small


# Step 3: Perform Hotelling's T2 test for large sample
T2_large <- hotelling.test(data_large, t(as.matrix(mu)))
T2_large

# Step 4: Display results
cat("Hotelling's T2 test result for small sample:\n")
print(T2_small)

cat("\nHotelling's T2 test result for large sample:\n")
print(T2_large)
