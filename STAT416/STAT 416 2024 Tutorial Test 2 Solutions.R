##Example 4
xbar <- c(85, 78, 73, 90)
mu <- c(80, 75, 70, 85)
S <- matrix(c(15, 3, 2, 5,
                         3, 12, 4, 2,
                         2, 4, 10, 3,
                         5, 2, 3, 9), nrow = 4, byrow = TRUE)
S
inv_S=solve(S)
inv_S
n<- 40  # Sample size
p <- length(xbar)

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
  p <- length(sample_mean)
  df1 <- p
  df2 <- n - p
  
  
  # Compute the F-statistic
  #f_stat <- (df2 / (p * (n - 1))) * t2_stat
  #f_stat <- t2_stat
  
  # Critical value for F distribution
  alpha <- 0.05  # Set significance level
  f_crit <- ((p * (n - 1))/df2)*qf(1 - alpha, df1, df2)
  
  # Decision
  decision <- ifelse(t2_stat > f_crit, "Reject H0", "Fail to reject H0")
  
  return(list(T2 = t2_stat,  F_critical = f_crit, decision = decision))
}


# Perform Hotelling's T² test for each dataset
result_1 <- hotelling_test(xbar, mu, S, n)
print(result_1)


########Confidence Intervals

##Simultaneous Confidence Intervals
T.ci <- function(mu, Sigma, n, avec=rep(1,length(mu)), level=0.95){
  p <- length(mu)
  if(nrow(Sigma)!=p) stop("Need length(mu) == nrow(Sigma).")
  if(ncol(Sigma)!=p) stop("Need length(mu) == ncol(Sigma).")
  if(length(avec)!=p) stop("Need length(mu) == length(avec).")
  if(level <=0 | level >= 1) stop("Need 0 < level < 1.")
  zhat <- crossprod(avec, mu)
  df1 <- p
  df2 <- n - p
  if(length(n)==1L){
    cval <- ((p * (n - 1))/df2)*qf(1 - alpha, df1, df2)
    zvar <- crossprod(avec, Sigma %*% avec) / n
  } 
  const <- sqrt(cval * zvar)
  c(lower = zhat - const, upper = zhat + const)
}




TCI<- bon <- NULL
alpha <- 1 - (0.05/(2*p))
for(k in 1:p){
  avec <- rep(0, p)
  avec[k] <- 1
  TCI <- c(TCI, T.ci(xbar, S, n, avec, 0.95))
  
  bon <- c(bon,
           xbar[k] - sqrt(S[k,k]/n) * qt(alpha,df=n-1),
           xbar[k] + sqrt(S[k,k]/n) * qt(alpha,df=n-1))
}

rtab <- rbind(TCI, bon)
round(rtab, 3)

