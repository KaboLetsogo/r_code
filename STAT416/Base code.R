### MANOVA CODE

decompose_groups <- function(groups) {
  # Calculate the overall mean of all observations
  all_observations <- unlist(groups)  # Flatten the list of groups into a single vector
  overall_mean <- mean(all_observations)
  
  # Calculate the mean for each group
  group_means <- lapply(groups, mean)
  
  # Initialize lists to store treatment and error values for each group
  treatment_list <- list()
  error_list <- list()
  
  # Loop over each group to calculate treatment and error lists
  for (i in seq_along(groups)) {
    group <- groups[[i]]
    group_mean <- group_means[[i]]
    
    # Treatment: Difference between group mean and overall mean, replicated for each observation in the group
    treatment_list[[i]] <- rep(group_mean - overall_mean, length(group))
    
    # Error: Difference between each observation and the group mean
    error_list[[i]] <- group - group_mean
  }
  
  # Name the lists for clarity
  names(treatment_list) <- names(error_list) <- paste0("group", seq_along(groups))
  
  # Return the treatment and error lists
  list(
    overall_mean = overall_mean,
    treatment = treatment_list,
    error = error_list
  )
}

calculate_sums_of_squares <- function(groups) {
  # Flatten the list of groups into a single vector of all observations
  all_observations <- unlist(groups)
  
  # Step 1: Calculate the overall mean of all observations
  overall_mean <- mean(all_observations)
  
  # Step 2: Calculate the mean for each group
  group_means <- lapply(groups, mean)
  
  # Initialize sum of squares variables
  SStr <- 0  # Treatment (between-group) sum of squares
  SSres <- 0 # Error (within-group) sum of squares
  
  # Step 3: Loop over each group to calculate SStr and SSres
  for (i in seq_along(groups)) {
    group <- groups[[i]]
    group_mean <- group_means[[i]]
    n_j <- length(group)
    
    # Treatment Sum of Squares (SStr): Between-group variability
    SStr <- SStr + n_j * (group_mean - overall_mean)^2
    
    # Error Sum of Squares (SSres): Within-group variability
    SSres <- SSres + sum((group - group_mean)^2)
  }
  
  # Step 4: Calculate SSmean based on the total number of observations
  n_total <- length(all_observations)
  SSmean <- n_total * (overall_mean)^2
  
  # Step 5: Calculate SSobs as SSmean + SStr + SSres
  SSobs <- SSmean + SStr + SSres
  
  # Step 6: Calculate SStotal as SSobs - SSmean
  SStotal <- SSobs - SSmean
  
  # Output the results as a list
  list(
    SStr = SStr,      # Treatment sum of squares (between-group)
    SSres = SSres,    # Error sum of squares (within-group)
    SSobs = SSobs,    # Sum of squares for observations
    SSmean = SSmean,  # Sum of squares for the mean
    SStotal = SStotal # Corrected total sum of squares
  )
}

calculate_cross_products <- function(groups_var1, groups_var2) {
  # Flatten the lists into vectors of all observations
  all_observations1 <- unlist(groups_var1)
  all_observations2 <- unlist(groups_var2)
  
  # Step 1: Calculate overall means
  overall_mean1 <- mean(all_observations1)
  overall_mean2 <- mean(all_observations2)
  
  # Step 2: Calculate group means
  group_means1 <- lapply(groups_var1, mean)
  group_means2 <- lapply(groups_var2, mean)
  
  # Step 3: Calculate the Mean Cross Product
  n_total <- length(all_observations1)
  mean_cross_product <- n_total * overall_mean1 * overall_mean2
  
  # Initialize cross product sums
  treatment_cross_product <- 0
  residual_cross_product <- 0
  total_cross_product <- 0
  
  # Step 4: Calculate Treatment, Residual, and Total Cross Products
  for (i in seq_along(groups_var1)) {
    group1 <- groups_var1[[i]]
    group2 <- groups_var2[[i]]
    group_mean1 <- group_means1[[i]]
    group_mean2 <- group_means2[[i]]
    n_j <- length(group1)
    
    # Treatment Cross Product
    treatment_cross_product <- treatment_cross_product + n_j * (group_mean1 - overall_mean1) * (group_mean2 - overall_mean2)
    
    # Residual Cross Product
    residual_cross_product <- residual_cross_product + sum((group1 - group_mean1) * (group2 - group_mean2))
    
    # Total Cross Product
    total_cross_product <- total_cross_product + sum(group1 * group2)
  }
  
  # Step 5: Calculate the Total (Corrected) Cross Product
  corrected_cross_product <- total_cross_product - mean_cross_product
  
  # Output the results
  list(
    Mean_Cross_Product = mean_cross_product,
    Treatment_Cross_Product = treatment_cross_product,
    Residual_Cross_Product = residual_cross_product,
    Total_Cross_Product = total_cross_product,
    Corrected_Cross_Product = corrected_cross_product
  )
}

create_manova_table <- function(SSresults_var1, SSresults_var2, cross_product_results, groups) {
  # Calculate group sizes based on input groups
  group_sizes <- sapply(groups, length)
  
  # Extract sums of squares and cross products
  SStr1 <- SSresults_var1$SStr      # Treatment sum of squares for variable 1
  SSres1 <- SSresults_var1$SSres     # Residual sum of squares for variable 1
  SStotal1 <- SSresults_var1$SStotal # Total sum of squares for variable 1
  
  SStr2 <- SSresults_var2$SStr      # Treatment sum of squares for variable 2
  SSres2 <- SSresults_var2$SSres     # Residual sum of squares for variable 2
  SStotal2 <- SSresults_var2$SStotal # Total sum of squares for variable 2
  
  CrossProd_treatment <- cross_product_results$Treatment_Cross_Product
  CrossProd_residual <- cross_product_results$Residual_Cross_Product
  CrossProd_total <- cross_product_results$Corrected_Cross_Product
  
  # Calculate degrees of freedom
  g <- length(group_sizes)  # Number of groups
  df_treatment <- g - 1
  df_residual <- sum(group_sizes) - g
  df_total <- sum(group_sizes) - 1
  
  # Create matrices
  treatment_matrix <- matrix(c(SStr1, CrossProd_treatment, 
                               CrossProd_treatment, SStr2), nrow = 2, byrow = TRUE)
  
  residual_matrix <- matrix(c(SSres1, CrossProd_residual, 
                              CrossProd_residual, SSres2), nrow = 2, byrow = TRUE)
  
  total_matrix <- matrix(c(SStotal1, CrossProd_total, 
                           CrossProd_total, SStotal2), nrow = 2, byrow = TRUE)
  
  # Compile MANOVA table with matrices and degrees of freedom
  manova_table <- list(
    Treatment = list(Matrix = treatment_matrix, df = df_treatment),
    Residual = list(Matrix = residual_matrix, df = df_residual),
    Total = list(Matrix = total_matrix, df = df_total)
  )
  
  return(manova_table)
}

calculate_wilks_lambda <- function(manova_table, p, alpha = 0.05) {
  # Extract the treatment and residual matrices and degrees of freedom
  treatment_matrix <- manova_table$Treatment$Matrix
  residual_matrix <- manova_table$Residual$Matrix
  df_treatment <- manova_table$Treatment$df
  df_residual <- manova_table$Residual$df
  
  # Step 1: Calculate Wilks' Lambda (Λ)
  lambda <- det(residual_matrix) / det(residual_matrix + treatment_matrix)
  
  # Step 2: Calculate the transformed test statistic (Λ*)
  g <- df_treatment + 1   # Number of groups
  N <- df_residual + g    # Total number of observations
  
  # Transformed test statistic based on number of variables (p) and groups (g)
  if (p == 1) {
    # For univariate case (p = 1), the transformation is different
    transformed_lambda <- ((1 - lambda) / lambda) * ((N - g) / df_treatment)
    f_df1 <- df_treatment
    f_df2 <- N - g
  } else if (g >= 3) {
    # General multivariate case
    transformed_lambda <- ((1 - sqrt(lambda)) / sqrt(lambda)) * ((N - p - 2) / p)
    f_df1 <- 2 * p
    f_df2 <- 2 * (N - p - 2)
  } else if (g == 2) {
    # For two groups with multiple variables
    transformed_lambda <- ((1 - lambda) / lambda) * ((N - p - 1) / p)
    f_df1 <- p
    f_df2 <- N - p - 1
  } else {
    stop("Invalid combination of p and g for this MANOVA test.")
  }
  
  # Step 3: Calculate the critical F-value
  critical_f <- qf(1 - alpha, df1 = f_df1, df2 = f_df2)
  
  # Step 4: Determine whether to reject the null hypothesis
  reject_null <- transformed_lambda > critical_f
  
  # Output results
  list(
    Wilks_Lambda = lambda,
    Transformed_Test_Statistic = transformed_lambda,
    Critical_F_Value = critical_f,
    Reject_Null_Hypothesis = reject_null
  )
}

# Example usage
group1_var1 <- c(9, 6, 9)
group2_var1 <- c(0, 2)
group3_var1 <- c(3, 1, 2)

group1_var2 <- c(3, 2, 7)
group2_var2 <- c(4, 0)
group3_var2 <- c(8, 9, 7)

p  <- 2
alpha <- 0.05

groups_var1 <- list(group1_var1, group2_var1, group3_var1)
groups_var2 <- list(group1_var2, group2_var2, group3_var2)

decomposition1 <- decompose_groups(groups_var1)
decomposition2 <- decompose_groups(groups_var2)

SSresults_var1 <- calculate_sums_of_squares(groups_var1)
SSresults_var2 <- calculate_sums_of_squares(groups_var2)

cross_product_results <- calculate_cross_products(groups_var1, groups_var2)

manova_table_results <- create_manova_table(SSresults_var1, SSresults_var2, cross_product_results, groups_var1)

manova_table_results

results_wilks <- calculate_wilks_lambda(manova_table_results, p = p, alpha = alpha)

results_wilks




residual_matrix = matrix(data = c(855,285,228,285,570,171,228,171,456),nrow=3,ncol=3)
treatment_matrix =matrix(data = c(644.2,492.2,314.2,492.2,369.4,237.4,314.2,237,4,155.4),nrow=3,ncol=3)

lambda <- det(residual_matrix) / det(residual_matrix + treatment_matrix)
lambda
N=60
p=3
transformed_lambda <- ((1 - sqrt(lambda)) / sqrt(lambda)) * ((N- p - 2) / p)
f_df1 <- 2 * p
f_df2 <- 2 * (N - p - 2)


critical_f <- qf(1 - alpha, df1 = f_df1, df2 = f_df2)

# Step 4: Determine whether to reject the null hypothesis
reject_null <- transformed_lambda > critical_f


