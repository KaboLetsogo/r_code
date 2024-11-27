###Notes
#What Does the λ Parameter Do in the Box-Cox Transformation formula?
#The λ parameter is critical in determining how the data are transformed:

#λ=1: No transformation is applied (identity transformation).

#λ=0: Logarithmic transformation is applied (log transformation).

#λ>1: The data is transformed to reduce positive skewness (making the distribution more symmetric).

#λ<1 but λ not =0: This transformation helps to reduce negative skewness (e.g., squaring values for negative skew).

#λ<0: Can reverse the direction of skewness for more extreme transformations (e.g., inverse transformation).


#How to Choose Transformation:
#Positive skew (right tail longer): Consider log
#However, cannot be used on Zero or negative values.

#For count data or data with moderate positive skewness: Consider sqrt transformations 

#Negative skew (left tail longer): Consider square transformation.

#######################################################################################
library(readr)
data <- read_csv("STAT416/framingham dataset.csv")
head(data)
dim(data)


####QUESTION 1
# a) Calculate the proportion of missing data for specified variables
missing_data=colSums(is.na(data))# for all variables
missing_data
missing_proportion <- colSums(is.na(data)) / nrow(data)
missing_proportion[c("BMI", "totChol", "diaBP",  "sysBP")]# for the required variables

#omit missing data
data<-na.omit(data)
#re check if missing values are omitted
missing_data=colSums(is.na(data))
dim(data)

# Install and load the MVN package
if(!require(MVN)) install.packages("MVN")
library(MVN)

# b) Test for multivariate normality
mvn_result <- mvn(data = data[, c("BMI", "totChol", "diaBP", "sysBP")], mvnTest = "mardia",
                  showOutliers = T, bc = T)
mvn_result



####Transform the data and do the multivariate normality test again
library(MASS)
#Transforming the data using (y^λ – 1) / λ
data$newBMI=((data$BMI)^-0.50-1)/-0.50
data$newtotChol=log(data$totChol)
data$newdiaBP=log(data$diaBP)
data$newsysBP=((data$sysBP)^-1.15-1)/-1.15


head(data)
mvn_result <- mvn(data = data[, c("newBMI", "newtotChol","newdiaBP", "newsysBP")], mvnTest = "hz",
                  showOutliers = T, bc=T)
mvn_result

#c) There are no multivariate outliers.

#d) We can continue with the analysis because the sample size is ratively large
#so we resort to the central limit thereom.


###############Question 3 
library(MASS)

#a) Hypothesis Test
# Hypothesized values
H0_values <- c(120, 80, 25, 100)

# Compute sample means and covariance matrix for specified variables
Xbars <- colMeans(data[, c("sysBP", "diaBP", "BMI","glucose" )])
Xbars
S<- cov(data[, c("sysBP", "diaBP", "BMI","glucose")])
solve(S)
n <- nrow(data) # sample size
n
p=length(Xbars)
p

# Hotelling's T^2 statistic
T2_stat <- n * t(Xbars-H0_values) %*% solve(S) %*% (Xbars-H0_values)

# Critical value and p-value
F_critical <- ((n - 1) * p / (n - p)) * qf(0.95, p, n - p)

p_value <- 1 - pf(T2_stat / ((n - 1) * p / (n - p)), p, n - p)

#Decision level
decision <- ifelse(T2_stat > F_critical, "Reject H0", "Fail to reject H0")

list(T2_statistic = T2_stat, F_critical = F_critical, p_value = p_value, decision )

#b) 95% T^2 Simultaneous Confidence Intervals
df1=p
df2=n-p
T.ci <- function(mu, Sigma, n, avec=rep(1,length(mu)), level=0.95){
  p <- length(mu)
  if(nrow(Sigma)!=p) stop("Need length(mu) == nrow(Sigma).")
  if(ncol(Sigma)!=p) stop("Need length(mu) == ncol(Sigma).")
  if(length(avec)!=p) stop("Need length(mu) == length(avec).")
  if(level <=0 | level >= 1) stop("Need 0 < level < 1.")
  fhat <- crossprod(avec, mu)
  if(length(n)==1L){
    cval <- ((n-1)*p/(n-p))*qf(0.95, df1=p,df2=n-p)
    fvar <- crossprod(avec, Sigma %*% avec) / n
  } 
  const <- sqrt(cval * fvar)
  c(lower = fhat - const, upper = fhat + const)
}


TCI<- bon <- NULL
alpha <- 1 - (0.05/(2*p))
for(k in 1:p){
  avec <- rep(0, p)
  avec[k] <- 1
  TCI <- c(TCI, T.ci(Xbars, S, n, avec))
  
  bon <- c(bon,
           Xbars[k] - sqrt(S[k,k]/n) * qt(alpha,df=n-1),
           Xbars[k] + sqrt(S[k,k]/n) * qt(alpha,df=n-1))
}

rtab <- rbind(TCI, bon)
round(rtab, 3)

###(e) Simulation Studies

# Set seed for reproducibility
set.seed(100)



# Parameters for the simulation
n <- 20    # Sample size (number of pairs)
p <- p     # Number of variables (e.g., systolic blood pressure, diastolic blood pressure, BMI, glucose)
Xbars # True difference in means (can adjust for effect size) #She said different in class
H0_values #If you had not/failed to reject H_0 you would use this instead of XBars in the mvrnorm below
sigma <- S # Covariance matrix (for before and after or paired differences)
alpha <- 0.05  # Significance level
simulations <- 10000  # Number of simulations
rejects <- 0  # Counter for rejections

# Critical F-value for Hotelling's T^2
F_critical <- ((p * (n - 1))/(n-p))*qf(1 - alpha, p, n - p)

# Simulate data and perform sensitivity analysis
for (i in 1:simulations) {
  # Simulate paired measurements (before and after) as two correlated samples
  #X_before <- mvrnorm(n, rep(0,p), sigma)  # Simulate the "before" condition
  X<- mvrnorm(n, Xbars, sigma)  # Simulate the "after" condition with a mean shift
  
  # Compute differences for the paired test
  D <- colMeans(X) - H0_values
  
  # Compute the paired T^2 statistic
  T2=n * t(D) %*% solve(sigma) %*% (D)
  
  # Check if the null hypothesis (no difference) is rejected
  if (T2 > F_critical) {
    rejects <- rejects + 1
  }
}

# Estimate the power of the test
power_estimate <- rejects / simulations
cat("Estimated power of the paired T^2 test:", power_estimate, "\n")




##########################################Question 4
data$education=as.factor(data$education)
head(data)

# Split data by levels of 'thal'
data_split <- split(data, data$education)

table(data$education)

# Calculate and print sample means for each level of 'thal'
Ybar <- lapply(data_split, function(subset) {
  colMeans(subset[, c( "sysBP", "diaBP", "totChol")])
})

print(Ybar)



# Calculate and print covariance matrix for each level of 'thal'
cov_matrices <- lapply(data_split, function(subset) {
  cov(subset[, c("sysBP", "diaBP", "totChol")])
})

print(cov_matrices)


###(a) Equality of Covariance Matrices
inst
library(heplots)

BM <- boxM(cbind(sysBP, diaBP, totChol) ~ education, data =data)
print(BM)

nrow(data)


############MANOVA
library(car)
fit <- lm(cbind(sysBP, diaBP, totChol) ~ education, data =data)
manova_model=Manova(fit, type = "II")

summary(manova_model)
#dim(filtered_data)
# Using a manova function in Base R
# Fit MANOVA model
manova_model1 <- manova(cbind(trestbps, chol, oldpeak) ~ thal, data = filtered_data)

summary(manova_model1)


########Computing W and B
print(cov_matrices)
Ybar

table(data$education)
n1=1526
n2=1101
n3=608
n4=423
g=4

n_total=n1+n2+n3+n4


#Computing W
W=(n1-1)*cov_matrices$`1` + (n2-1)*cov_matrices$`2` + (n3-1)*cov_matrices$`3` + (n4-1)*cov_matrices$`4`
W


#Computing B
# Calculate the overall mean vector
overall_mean <- (n1 * Ybar$`1` + n2 * Ybar$`2` + n3 * Ybar$`3`+n4*Ybar$`4`) / n_total
overall_mean


# Calculate the between-group sum of squares and cross-products matrix (B)
B <- n1 * (Ybar$`1` - overall_mean) %*% t(Ybar$`1` - overall_mean) +
  n2 * (Ybar$`2` - overall_mean) %*% t(Ybar$`2` - overall_mean) +
  n3 * (Ybar$`3` - overall_mean) %*% t(Ybar$`3` - overall_mean) +
  n4 * (Ybar$`4` - overall_mean) %*% t(Ybar$`4` - overall_mean)
B

TSS=B+W
TSS

#Now calculate Wilk's Lambda

wilks_lambda=det(W)/det(B+W)




#b) 95% Bonferroni confidence intervals

# Pooled within-group covariance matrix (S_w)
S_w <- W /(n_total - g)
S_w

# Compute standard errors for each variable in each pairwise comparison
se_1 <- sqrt(diag(S_w) * (1/n1 + 1/n4))
se_1


# Calculate 95% Bonferroni confidence intervals for each variable
# Bonferroni-corrected critical value
alpha <- (0.05) / (p*g*(g-1))
alpha

ci_bounds1 <- se_1 * qt(1 - alpha, (n_total-g))
ci_bounds1

# Calculate Bonferroni confidence intervals for each pairwise comparison
# fixed defect vs normal
diff_1 <-  Ybar$`4`-Ybar$`1`

ci_1 <- data.frame(
  Variable = c("trestbps", "chol",  "oldpeak"),
  Lower = diff_1 - ci_bounds1,
  Upper = diff_1 + ci_bounds1
)
ci_1

