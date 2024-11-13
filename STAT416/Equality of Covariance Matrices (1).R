library(MASS)

# Define sample sizes
n1 <- 271
n2 <- 138
n3 <- 107
N=n1+n2+n3
N
g=3
p=4

# Define mean vectors
X_bar_1 <- c(2.066, 0.480, 0.082, 0.360)
X_bar_1
X_bar_2 <- c(2.167, 0.596, 0.124, 0.418)
X_bar_2
X_bar_3 <- c(2.273, 0.521, 0.125, 0.383)
X_bar_3

# Define sample covariance matrices
S1 <- matrix(c(
  0.291, -0.001, 0.002, 0.010,
  -0.001, 0.011, 0.003, 0.000,
  0.002, 0.000, 0.001, 0.000,
  0.010, 0.003, 0.000, 0.010
), nrow = 4, byrow = TRUE)
S1

S2 <- matrix(c(
  0.561, 0.011, 0.001, 0.037,
  0.011, 0.025, 0.004, 0.007,
  0.001, 0.004, 0.005, 0.002,
  0.037, 0.007, 0.002, 0.019
), nrow = 4, byrow = TRUE)
S2

S3 <- matrix(c(
  0.261, 0.030, 0.003, 0.018,
  0.030, 0.017, -0.000, 0.006,
  0.003, -0.000, 0.004, 0.001,
  0.018, 0.006, 0.001, 0.013
), nrow = 4, byrow = TRUE)

S3
# Calculate pooled within-group covariance matrix W
W <- (n1 - 1) * S1 + (n2 - 1) * S2 + (n3 - 1) * S3
W

# Calculate overall mean vector X_bar
X_bar <- (n1 * X_bar_1 + n2 * X_bar_2 + n3 * X_bar_3) /N
X_bar

# Calculate between-group sum of squares matrix B
B <- n1 * (X_bar_1 - X_bar) %*% t(X_bar_1 - X_bar) +
  n2 * (X_bar_2 - X_bar) %*% t(X_bar_2 - X_bar) +
  n3 * (X_bar_3 - X_bar) %*% t(X_bar_3 - X_bar)
B

#Calculate the M_Box Statistic:
#Calculate M
Sp=W/(N-g)
Sp



M=(((n1-1)+(n2-1)+(n3-1))*log(det(Sp))
   -((n1-1)*log(det(S1))+(n2-1)*log(det(S2))+(n3-1)*log(det(S3)))
)
M

#Calculate u
u=((1/(n1-1)+1/(n2-1)+1/(n3-1))-1/((n1-1)+(n2-1)+(n3-1))
*(((2*(p^2))+(3*p)-1)/(6*(p+1)*(g-1)))
)
u

#Now calculate C:
C=(1-u)*M
C

#Critical Value
v=(1/2)*p*(p + 1)*(g - 1)
v
cv=qchisq(0.95,v)
cv


