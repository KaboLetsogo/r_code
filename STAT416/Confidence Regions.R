# Load necessary libraries
library(ellipse)



# Given data
n <- 42  # Sample size
x_bar <- c(0.564, 0.603)  # Sample mean vector
mu_0=c(0.562, 0.589)
D=x_bar-mu_0
D
S <- matrix(c(0.0144, -0.0117, -0.0117, 0.0146), nrow = 2)  # Covariance matrix
n*(D%*%solve(S)%*%D)
det(S)

# Calculate the critical value for the confidence ellipse
F_value=qf(0.95, df1=2, df2=40)
#F_value <- 3.23  # Critical value from the F-distribution for F(2, 40, 0.05)
c_squared <- (2 * (n - 1) / (n - 2)) * F_value
c_squared
# Generate the ellipse
confidence_ellipse <- ellipse(S, center = x_bar, t = sqrt(c_squared), level = 0.95)

#?ellipse()

# Plot the ellipse
plot(confidence_ellipse, type = "l", lwd = 2, col = "blue",
     xlab = expression(mu[1]), ylab = expression(mu[2]),
     main = "95% Confidence Ellipse for the Mean Vector")
points(x_bar[1], x_bar[2], col = "red", pch = 19)  # Mark the sample mean
text(x_bar[1], x_bar[2], labels = expression(bar(x)), pos = 3, col = "red")

# Add the test point
test_point <- c(0.562, 0.589)
points(test_point[1], test_point[2], col = "green", pch = 19)
text(test_point[1], test_point[2], labels = expression(mu["'"]), pos = 3, col = "green")

# Add legend
legend("topright", legend = c("95% Confidence Ellipse", "Sample Mean", "Test Point"),
       col = c("blue", "red", "green"), lwd = c(2, NA, NA), pch = c(NA, 19, 19))

######################################################################################
#The covariance determies the shape and the orientation of the ellipse
S <- matrix(c(0.0144, -0.0117, -0.0117, 0.0146), nrow = 2)  # Covariance matrix
S <- matrix(c(0.0144, 0, 0, 0.0146), nrow = 2)  # Covariance matrix

# Generate the ellipse
confidence_ellipse <- ellipse(S, center = x_bar, t = sqrt(c_squared), level = 0.95)


# Plot the ellipse
plot(confidence_ellipse, type = "l", lwd = 2, col = "blue",
     xlab = expression(mu[1]), ylab = expression(mu[2]),
     main = "95% Confidence Ellipse for the Mean Vector")
points(x_bar[1], x_bar[2], col = "red", pch = 19)  # Mark the sample mean
text(x_bar[1], x_bar[2], labels = expression(bar(x)), pos = 3, col = "red")

# Add the test point
test_point <- c(0.562, 0.589)
points(test_point[1], test_point[2], col = "green", pch = 19)
text(test_point[1], test_point[2], labels = expression(mu["'"]), pos = 3, col = "green")

# Add legend
legend("topright", legend = c("95% Confidence Ellipse", "Sample Mean", "Test Point"),
       col = c("blue", "red", "green"), lwd = c(2, NA, NA), pch = c(NA, 19, 19))




####################################################################################
# Example 3
sample_mean_3 <- c(78, 82, 86)
hypothesized_mean_3 <- c(75, 80, 85)
cov_matrix_3 <- matrix(c(10, 4, 3,
                         4, 8, 2,
                         3, 2, 9), nrow = 3, byrow = TRUE)
n_3 <- 35  # Sample size



##Example 4
sample_mean_4 <- c(85, 78, 73, 90)
hypothesized_mean_4 <- c(80, 75, 70, 85)
cov_matrix_4 <- matrix(c(15, 3, 2, 5,
                         3, 12, 4, 2,
                         2, 4, 10, 3,
                         5, 2, 3, 9), nrow = 4, byrow = TRUE)
n_4 <- 40  # Sample size


# Example 5
sample_mean_5 <- c(205, 195, 185, 215, 225)
hypothesized_mean_5 <- c(200, 190, 180, 210, 220)
cov_matrix_5 <- matrix(c(25, 3, 4, 5, 6,
                         3, 20, 2, 3, 4,
                         4, 2, 22, 2, 3,
                         5, 3, 2, 18, 4,
                         6, 4, 3, 4, 19), nrow = 5, byrow = TRUE)
n_5 <- 50  # Sample size





# Function to compute Hotelling's T² statistic
hotelling_test <- function(sample_mean, hypothesized_mean, cov_matrix, n) {

    # Compute the difference between sample mean and hypothesized mean
  diff_mean <- sample_mean - hypothesized_mean
  
  # Inverse of the covariance matrix
  inv_cov <- solve(cov_matrix)
  
  # Compute Hotelling's T² statistic
  t2_stat <- n * t(diff_mean) %*% inv_cov %*% diff_mean
  t2_stat <- as.numeric(t2_stat) # Convert matrix to scalar
  
  # Degrees of freedom
  p <- length(sample_mean_3)
  df1 <- p
  df2 <- n - p
  
  # Compute the F-statistic
  f_stat <- (df2 / (p * (n - 1))) * t2_stat
  
  # Critical value for F distribution
  alpha <- 0.05  # Set significance level
  f_crit <- qf(1 - alpha, df1, df2)
  
  # Decision
  decision <- ifelse(f_stat > f_crit, "Reject H0", "Fail to reject H0")
  
  return(list(T2 = t2_stat, F_value = f_stat, F_critical = f_crit, decision = decision))
}


# Perform Hotelling's T² test for each dataset
result_1 <- hotelling_test(sample_mean_3, hypothesized_mean_3, cov_matrix_3, n_3)
result_2 <- hotelling_test(sample_mean_4, hypothesized_mean_4, cov_matrix_4, n_4)
result_3 <- hotelling_test(sample_mean_5, hypothesized_mean_5, cov_matrix_5, n_5)

# Print the results
print("Results for example 3:")
print(result_1)

print("Results for example 4:")
print(result_2)

print("Results for example 5:")
print(result_3)
