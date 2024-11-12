effluent_data <- data.frame(
  Sample = 1:11,
  Commercial_Lab_BOD = c(),
  Commercial_Lab_SS = c(),
  State_Lab_BOD = c),
  State_Lab_SS = c()
)


effluent_data <- mutate(effluent_data,D_BOD = Commercial_Lab_BOD-State_Lab_BOD,
                        D_SS = Commercial_Lab_SS - State_Lab_SS)


covMat = cov(dplyr::select(effluent_data,D_BOD,D_SS))
covMat

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
  f_stat <- (df2 / (p * (n - 1))) * t2_stat
  #f_stat <- t2_stat
  
  # Critical value for F distribution
  alpha <- 0.05  # Set significance level
  f_crit <- ((p * (n - 1))/df2)*qf(1 - alpha, df1, df2)
  
  # Decision
  decision <- ifelse(f_stat > f_crit, "Reject H0", "Fail to reject H0")
  
  return(list(T2 = t2_stat, F_value = f_stat, F_critical = f_crit, decision = decision))
}
#Q2
xbar_b = matrix(data=c(120,80,25),nrow = 3,ncol = 1)
xbar_a = matrix(data=c(115,78,23),nrow = 3,ncol = 1)

xbar = xbar_b - xbar_a
mu = matrix(data=c(0,0,0),nrow = 3,ncol = 1)

S = matrix(data = c(), nrow=3, ncol=3)

nyyyyyy

###
xbar = matrix(data=c(85,78,73,90),nrow = ,ncol = 1)

mu = matrix(data=c(80,75,70,85),nrow = 4,ncol = 1)

S = matrix(data=c(15,3,2,5,3,12,4,2,2,4,10,3,5,2,3,9),nrow = 4,ncol = 4)

n = 40 

p=4

#qf(0.05,4,36,lower.tail=TRUE)

# Perform Hotelling's T² test for each dataset
result_1 <- hotelling_test(xbar, mu, S,n)
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
  if(length(n)==1L){
    cval <- qchisq(0.95, df=p)
    zvar <- crossprod(avec, Sigma %*% avec) / n
  } 
  const <- sqrt(cval * zvar)
  c(lower = zhat - const, upper = zhat + const)
}
T.ci(mu,S,40)



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

#######################################


# Function to perform Hotelling's T² test for independent samples
hotelling_independent_test <- function(alpha = 0.05) {
  # Calculate sample sizes and means
  n1 <- 25
  n2 <- 25
  mean1 <- matrix(data=c(130,85,28),nrow = 3,ncol = 1)
  mean2 <- matrix(data=c(125,82,26),nrow = 3,ncol = 1)
  p=3
  
  pooled_cov <- matrix(data=c(12,3,4,4,9,2,4,2,11),nrow = 3,ncol = 3)
  mean_diff <- mean1 - mean2
  t2_stat <- (n1 * n2 / (n1 + n2)) * t(mean_diff) %*% solve(pooled_cov) %*% mean_diff
  t2_stat <- as.numeric(t2_stat)
  
  # F-statistic for homogeneous case
  df1 <- p
  df2 <- n1 + n2 - p - 1
  f_stat <- (df2 / (p * (n1 + n2 - 2))) * t2_stat
  f_crit <- ((p * (n1 + n2 - 2)) / df2) * qf(1 - alpha, df1, df2)
  
  # Decision
  decision <- ifelse(f_stat > f_crit, "Reject H0", "Fail to reject H0")
  
  list(T2 = t2_stat, F_value = f_stat, F_critical = f_crit, decision = decision)
}

hotelling_independent_test()


# Function to calculate confidence region and intervals for independent samples
confidence_intervals_independent <- function() {
  n1 <- 25
  n2 <- 25
  mean1 <- matrix(data=c(130,85,28),nrow = 3,ncol = 1)
  mean2 <- matrix(data=c(125,82,26),nrow = 3,ncol = 1)
  p=3
  alpha = 0.05
  
  pooled_cov <- matrix(data=c(12,3,4,4,9,2,4,2,11),nrow = 3,ncol = 3)
  mean_diff <- mean1 - mean2
  
  cov_matrix <- (1 / n1 + 1 / n2) * pooled_cov
  t2_crit <- qf(1 - alpha, p, n1 + n2 - p - 1)
    
  # Simultaneous confidence intervals
  simultaneous_ci <- list()
  for (j in 1:p) {
    se_simultaneous <- sqrt(t2_crit * cov_matrix[j, j])
    simultaneous_ci[[j]] <- c(mean_diff[j] - se_simultaneous, mean_diff[j] + se_simultaneous)
  }
  
  # Bonferroni confidence intervals
  bon_crit <- qt(1 - alpha / (2 * p), n1 + n2 - 2)
  bonferroni_ci <- list()
  for (j in 1:p) {
    se_bon <- sqrt(cov_matrix[j, j])
    bonferroni_ci[[j]] <- c(mean_diff[j] - bon_crit * se_bon, mean_diff[j] + bon_crit * se_bon)
  }
  
  # Confidence region
  confidence_region <- list(
    Lower = mean_diff - sqrt(t2_crit * diag(cov_matrix)),
    Upper = mean_diff + sqrt(t2_crit * diag(cov_matrix))
  )
  
  list(
    Confidence_Region = confidence_region,
    mean_diff = mean_diff,
    Simultaneous_CI = simultaneous_ci,
    Bonferroni_CI = bonferroni_ci
  )
}

confidence_intervals_independent()









