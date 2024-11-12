########################################Graphical Displays

# Load necessary library 
#install.packages('scatterplot3d')
#install.packages('rgl')
install.packages("GGally")
library(scatterplot3d) 
library(rgl)
library(tidyverse)
library(GGally)
# Example data: mtcars dataset 
data(mtcars)
#View(mtcars)
head(mtcars,10)



# Plot the data points and the confidence ellipse
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(alpha = 0.5) +
  stat_ellipse(level = 0.95, col = "red") +
  ggtitle("Scatter Plot with 95% Confidence Ellipse") +
  xlab("Weight(in 1000 tons)") +
  ylab("Miles per gallon")


# Plot the data points and the confidence ellipse
ggplot(mtcars, aes(x = wt, y = hp)) +
  geom_point(alpha = 0.5) +
  stat_ellipse(level = 0.95, col = "red") +
  ggtitle("Scatter Plot with 95% Confidence Ellipse") +
  xlab("Weight(in 1000 tons)") +
  ylab("Horse Power")


# 3D Scatterplot 
scatterplot3d(mtcars$wt, mtcars$mpg, mtcars$hp, pch = 19, color = mtcars$cyl)


# Scatterplot Matrix 
ggpairs(mtcars[, c("mpg", "disp", "hp", "wt")])


# Calculate the correlation matrix
cor_matrix <- cor(mtcars)

# Load the necessary library for better visualization (optional)
install.packages("pheatmap")  # Install pheatmap if not already installed
library(pheatmap)

# Create a heatmap of the correlation matrix
pheatmap(cor_matrix, 
         cluster_rows = TRUE, 
         cluster_cols = TRUE, 
         display_numbers = TRUE, 
         color = colorRampPalette(c("blue", "white", "red"))(50),
         main = "Correlation Heatmap of mtcars Data",
         angle_col = 45)


# Bubble Chart
ggplot(mtcars, aes(x = wt, y = mpg, size = hp, color = mtcars$cyl)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(3, 10)) +
  theme_minimal()

# Select a few rows and columns for demonstration
data_selected <- mtcars[1:10, c("mpg", "disp", "hp", "wt", "qsec")]

# Create the star plot
stars(data_selected, full = TRUE, draw.segments = TRUE,
      key.loc = c(15, 2), 
      main = "Star Plot of Selected Cars")


#library required for chernoff faces
install.packages("aplpack")
library(aplpack)

# Chernoff Faces
faces(mtcars[1:10, c("mpg", "disp", "hp", "wt")])


# Load necessary library for Andrews Curves
library(GGally)
library(ggplot2)
install.packages("reshape2")
library(reshape2)

# Example data: iris dataset
data(iris)


# Define Andrews curve function for a single observation
andrews_curve_single <- function(x) {
  t <- seq(-pi, pi, length.out = 100)
  curve <- 1/sqrt(2) * (x[1] + sin(t) * x[2] + cos(t) * x[3] + sin(2 * t) * x[4])
  return(curve)
}

# Initialize an empty matrix to hold Andrews curves
andrews_data <- matrix(NA, nrow = nrow(iris), ncol = 100)

# Calculate Andrews curves for each observation
for (i in 1:nrow(iris)) {
  andrews_data[i, ] <- andrews_curve_single(as.numeric(iris[i, 1:4]))
}

# Convert to data frame and add species information
andrews_data <- as.data.frame(andrews_data)
colnames(andrews_data) <- seq(-pi, pi, length.out = 100)
andrews_data$Species <- iris$Species

# Melt the data for ggplot
andrews_melted <- melt(andrews_data, id.vars = "Species")

# Plot Andrews curves
ggandrew <- ggplot(andrews_melted, aes(x = as.numeric(variable), y = value, color = Species, group = interaction(Species, 1:nrow(andrews_data)))) +
  geom_line() +
  xlab("t (Time)") + ylab("Andrews Curve Value") +
  theme_minimal()

ggandrew

# Example using a single observation (first row of iris dataset)
obs <- iris[1, ]

# Define the time variable
t <- seq(-pi, pi, length.out = 100)

# Calculate each component of the Andrews curve
sepal_length_component <- obs$Sepal.Length / sqrt(2)
y<-rep(sepal_length_component,100)
sepal_width_component <- obs$Sepal.Width * sin(t)
petal_length_component <- obs$Petal.Length * cos(t)
petal_width_component <- obs$Petal.Width * sin(2 * t)

# Plot each component
plot(t, y + sepal_width_component + petal_length_component + petal_width_component, type = "l", col = "black", ylim = c(-3, 10), ylab = "Andrews Curve Value", xlab = "t")
lines(t, y, col = "red", lty = 2) # Sepal length (constant)
lines(t, sepal_width_component, col = "blue", lty = 2) # Sepal width (sin(t))
lines(t, petal_length_component, col = "green", lty = 2) # Petal length (cos(t))
lines(t, petal_width_component, col = "purple", lty = 2) # Petal width (sin(2t))
legend("topright", legend = c("Combined", "Sepal Length", "Sepal Width", "Petal Length", "Petal Width"), col = c("black", "red", "blue", "green", "purple"), lty = 1:2)




######################################################otherplots
# Load necessary libraries world map
library(ggplot2)
library(igraph)
library(maps)

# Load the world map
world_map <- map_data("world")


# Example cities data
cities_data <- data.frame(
  city = c("New York", "London", "Sydney", "Tokyo", "San Francisco"),
  lat = c(40.7128, 51.5074, -33.8688, 35.6895, 37.7749),
  lon = c(-74.0060, -0.1278, 151.2093, 139.6917, -122.4194)
)


# Example friendships data
friendships_data <- data.frame(
  from_city = c("New York", "London", "Sydney", "Tokyo", "San Francisco"),
  to_city = c("London", "Sydney", "Tokyo", "San Francisco", "New York"),
  lon_from = c(-74.0060, -0.1278, 151.2093, 139.6917, -122.4194),
  lat_from = c(40.7128, 51.5074, -33.8688, 35.6895, 37.7749),
  lon_to = c(-0.1278, 151.2093, 139.6917, -122.4194, -74.0060),
  lat_to = c(51.5074, -33.8688, 35.6895, 37.7749, 40.7128),
  weight = c(1, 1, 1, 1, 1)  # Example weights, adjust as needed
)


# Create the plot
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "white", color = "gray") +
  geom_point(data = cities_data, aes(x = lon, y = lat), color = "blue", size = 1) +
  geom_curve(data = friendships_data, aes(x = lon_from, y = lat_from, xend = lon_to, yend = lat_to, alpha = weight), color = "lightblue") +
  theme_void() +
  theme(legend.position = "none")




# Install and load the fmsb package
install.packages("fmsb")
library(fmsb)

# Select some columns from mtcars for the radar chart
data <- mtcars[, c("mpg", "disp", "hp", "drat", "wt")]

# Add a row with maximum values
data_max <- apply(data, 2, max)

# Add a row with minimum values
data_min <- apply(data, 2, min)

# Combine the max and min with the actual data for plotting
data <- rbind(data_max, data_min, data[1:7,])  # Plotting only first 5 cars for simplicity

# Create the radar plot
radarchart(data, 
           axistype = 1, 
           pcol = c("red", "blue", "green", "purple", "orange","black", "pink"),  # Colors for each car
           #pfcol = c(rgb(1,0,0,0.2), rgb(0,0,1,0.2), rgb(0,1,0,0.2), rgb(0.5,0,0.5,0.2), rgb(1,0.5,0,0.2)),  # Transparent fill colors
           plwd = 2, 
           plty = 1, 
           title = "Radar Chart of Selected mtcars Variables")


# Create the radar plot
radarchart(data, 
           axistype = 1, 
           pcol = c("red", "blue", "green", "purple", "orange", "black", "pink", "yellow", "brown", "darkblue","lightblue"),  # Colors for each car
           #pfcol = c(rgb(1,0,0,0.2), rgb(0,0,1,0.2), rgb(0,1,0,0.2), rgb(0.5,0,0.5,0.2), rgb(1,0.5,0,0.2)),  # Transparent fill colors
           plwd = 2, 
           plty = 1, 
           title = "Radar Chart of Selected mtcars Variables")


# Install and load the GGally package
install.packages("GGally")
library(GGally)

# Load the mtcars dataset
data(mtcars)

# Select a subset of columns to visualize, including 'cyl' for grouping
selected_data <- mtcars[, c("mpg", "disp", "hp", "wt", "qsec", "cyl")]

# Create the Parallel Coordinates Plot
ggparcoord(data = selected_data,
           columns = 1:5,                # The columns to include in the plot
           groupColumn = "cyl",          # Group by 'cyl' (number of cylinders)
           scale = "uniminmax",          # Scaling method (uniminmax scales each variable to [0,1])
           title = "Parallel Coordinates Plot of Selected mtcars Variables",
           alphaLines = 0.5)             # Transparency of the lines



################################################################################

# Install ggbiplot package if you haven't installed it yet
# install.packages("devtools")
# devtools::install_github("vqv/ggbiplot")

# Load the necessary libraries
library(ggbiplot)
# Load the iris dataset
data(iris)

# Perform PCA on the first four columns of the iris dataset (ignoring the species column)
# Scale the data (centering and scaling) before PCA
iris.pca <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE)

# Summary of PCA
summary(iris.pca)
# Create a PCA biplot
ggbiplot(iris.pca, 
         obs.scale = 1, 
         var.scale = 1, 
         groups = iris$Species, 
         ellipse = TRUE, 
         circle = TRUE) +
  scale_color_discrete(name = '') +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "PCA Biplot of Iris Dataset")

# Plot PC1 vs PC3
ggbiplot(iris.pca, 
         choices = c(1, 3),  # Change to PC1 and PC3
         obs.scale = 1, 
         var.scale = 1, 
         groups = iris$Species, 
         ellipse = TRUE, 
         circle = TRUE) +
  scale_color_discrete(name = '') +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "PCA Biplot of Iris Dataset (PC1 vs PC3)")


# Plot PC2 vs PC3
ggbiplot(iris.pca, 
         choices = c(2, 3),  # Change to PC2 and PC3
         obs.scale = 1, 
         var.scale = 1, 
         groups = iris$Species, 
         ellipse = TRUE, 
         circle = TRUE) +
  scale_color_discrete(name = '') +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "PCA Biplot of Iris Dataset (PC2 vs PC3)")

####################################################################################################

# Example 1: Summation Method
data_matrix <- matrix(c(2, 4, 6, 
                        3, 5, 7, 
                        4, 6, 8), 
                      nrow = 3, byrow = TRUE)

# Summation method to calculate the mean of the second column (index 2)
column_index <- 2
column_data <- data_matrix[, column_index]
column_data
mean_summation <- sum(column_data) / length(column_data)

cat("Mean of column", column_index, "using summation method is:", mean_summation, "\n")




# Example 2: Matrix method
data_matrix <- matrix(c(2, 4, 6, 
                        3, 5, 7, 
                        4, 6, 8), 
                      nrow = 3, byrow = TRUE)
print(data_matrix)

# Matrix method to calculate the mean of the second column (index 2)
#column_index <- data_matrix[,2]
column_index<-2
column_vector <- data_matrix[, column_index]
column_vector
x <- c( 1, 2, NA, 4)
length(x)
x <- 1:3
x[-2]
x[0]

n <- length(column_vector)
n
ones_vector <- matrix(1, n, 1)
ones_vector
mean_matrix <- (1 / n) * t(ones_vector) %*% column_vector
mean_matrix

cat("Mean of column", column_index, "using matrix method:", mean_matrix[1], "\n")




#############variance covariance matrix
# Load necessary library
library(MASS)

# Set seed for reproducibility
set.seed(123)

# Number of assets in each portfolio
n_assets1 <- 6  # Portfolio 1 has 6 assets (3 large-cap, 2 small-cap, 1 international)
n_assets2 <- 6  # Portfolio 2 has 6 assets (2 bonds, 2 REITs, 2 commodities, 1 cash)

# Mean returns for each asset class
mu1 <- c(0.08, 0.07, 0.09, 0.10, 0.12, 0.06)  # Portfolio 1
mu2 <- c(0.03, 0.04, 0.05, 0.07, 0.08, 0.01)  # Portfolio 2

# Covariance matrices for the two portfolios
Sigma1 <- matrix(c(0.02, 0.001, 0.0015, 0.0005, 0.0007, 0.001,
                   0.001, 0.015, 0.001, 0.0005, 0.0007, 0.001,
                   0.0015, 0.001, 0.025, 0.001, 0.0009, 0.0012,
                   0.0005, 0.0005, 0.001, 0.018, 0.0011, 0.001,
                   0.0007, 0.0007, 0.0009, 0.0011, 0.030, 0.0015,
                   0.001, 0.001, 0.0012, 0.001, 0.0015, 0.010), nrow = 6, ncol = 6)

Sigma2 <- matrix(c(0.005, 0.0008, 0.0005, 0.0007, 0.0006, 0.0003,
                   0.0008, 0.007, 0.0006, 0.0008, 0.0005, 0.0004,
                   0.0005, 0.0006, 0.020, 0.001, 0.0012, 0.0007,
                   0.0007, 0.0008, 0.001, 0.015, 0.0009, 0.0006,
                   0.0006, 0.0005, 0.0012, 0.0009, 0.025, 0.0008,
                   0.0003, 0.0004, 0.0007, 0.0006, 0.0008, 0.002), nrow = 6, ncol = 6)




# Simulate returns for each portfolio
#R1 is a matrix where each row represents a simulated set of returns for all
#assets in your portfolio over a single period (e.g., a month, a year).

#Each column in R1 corresponds to the return of a specific asset in the portfolio.

#For example, if you run 1000 simulations, R1 will have 1000 rows 
#(one for each simulation) and as many columns as there are assets in your portfolio.
n_simulations <- 1000
R1 <- mvrnorm(n = n_simulations, mu = mu1, Sigma = Sigma1)# Returns for assets in Portfolio 1
R2 <- mvrnorm(n = n_simulations, mu = mu2, Sigma = Sigma2)# Returns for assets in Portfolio 2

propneg1=
  #Weights for the assets in each portfolio
  #w1 is a vector that represents the proportion of the total portfolio invested in 
  #each asset. The weights sum to 1 (or 100%).
  
  # Portfolio 1: 60% Large-Cap Stocks, 20% Small-Cap Stocks, 20% International Stocks
  # Portfolio 1 has 6 assets (3 large-cap, 2 small-cap, 1 international)
  w1 <- c(0.2, 0.2, 0.2, 0.1, 0.1, 0.2)  # Portfolio 1

# Portfolio 2: 40% Bonds, 30% REITs, 20% Commodities, 10% Cash Equivalents
# Portfolio 2 has 6 assets (2 bonds, 2 REITs, 2 commodities, 1 cash)
w2 <- c(0.2, 0.2, 0.15, 0.15, 0.05, 0.)  # Portfolio 2


#Option 1:
#Covariance Matrix: This matrix shows how each asset's return varies
#with every other asset's return and is independent of the portfolio weights.
cov_matrix <- cov(R1)
cov_matrix

portfolio_variance <- t(w1) %*% cov_matrix %*% w1
portfolio_variance <- as.numeric(portfolio_variance)  # Convert to a numeric value
portfolio_variance

cov_matrix2 <- cov(R2)
cov_matrix2

portfolio_variance2 <- t(w2) %*% cov_matrix %*% w2
portfolio_variance2 <- as.numeric(portfolio_variance)  # Convert to a numeric value
portfolio_variance2

#Option 2:
#This approach is especially useful when you want to understand the portfolio's
#performance under various scenarios. 

#R1 %*% w1: This operation computes the portfolio return for each simulation by 
#multiplying the simulated returns of each asset by its respective weight and 
#summing the results. 

#The output, portfolio1_returns, is a vector of simulated portfolio returns.

#var(portfolio1_returns): This function then calculates the variance of the 
#simulated portfolio returns, giving you an empirical measure of the portfolio's 
#risk based on the simulated scenarios.

# Portfolio returns
portfolio1_returns <- R1 %*% w1
portfolio2_returns <- R2 %*% w2


# Calculate the expected return
expected_return <- mean(portfolio1_returns)
expected_return

expected_return2 <- mean(portfolio2_returns)
expected_return2

# Calculate the number of losses (returns < 0)
number_of_losses <- sum(portfolio1_returns < 0)
number_of_losses

number_of_losses2 <- sum(portfolio2_returns < 0)
number_of_losses2

# Calculate the proportion of losses
proportion_of_losses <- number_of_losses / length(portfolio1_returns)
proportion_of_losses

proportion_of_losses2 <- number_of_losses2 / length(portfolio2_returns)
proportion_of_losses2




# Variance for portfolio 1
var_portfolio1 <- var(portfolio1_returns)

# Variance for portfolio 2
var_portfolio2 <- var(portfolio2_returns)

# Variance of the combined portfolio (assuming independence)
var_combined_portfolio <- var_portfolio1 + var_portfolio2

# Output the variances
var_portfolio1
var_portfolio2
var_combined_portfolio



# Simulate a single set of returns for illustration (not the full 1000 simulations)
RTrial <- mvrnorm(1, mu1, Sigma1)  # Returns for assets in Portfolio 1
portfolio_return <- sum(RTrial * w1)  # Calculate the portfolio return

# Check the return value
portfolio_return



