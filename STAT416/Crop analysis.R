# Load necessary libraries
library(readr)
library(tidyverse)
library(corrplot)
library(e1071)
library(gridExtra)
library(patchwork)



# Read the data
data <- read_csv("STAT416/simulated_crop_growth_data.csv")

# Convert Soil_Type to a factor
data$Soil_Type <- as.factor(data$Soil_Type)

# Question 1: Understanding Distributions
# a. Create histograms for the variables
histograms <- list(
  Rainfall = ggplot(data, aes(x = Rainfall)) + 
    geom_histogram(binwidth = 10, fill = "skyblue", color = "black") + 
    ggtitle("Histogram of Rainfall") + 
    xlab("Rainfall (mm)"),
  
  Temperature = ggplot(data, aes(x = Temperature)) + 
    geom_histogram(binwidth = 1, fill = "orange", color = "black") + 
    ggtitle("Histogram of Temperature") + 
    xlab("Temperature (°C)"),
  
  Sunlight = ggplot(data, aes(x = Sunlight)) + 
    geom_histogram(binwidth = 1, fill = "yellow", color = "black") + 
    ggtitle("Histogram of Sunlight") + 
    xlab("Sunlight (hours per day)"),
  
  Fertilizer = ggplot(data, aes(x = Fertilizer)) + 
    geom_histogram(binwidth = 5, fill = "green", color = "black") + 
    ggtitle("Histogram of Fertilizer Usage") + 
    xlab("Fertilizer (kg per hectare)"),
  
  Crop_Yield = ggplot(data, aes(x = Crop_Yield)) + 
    geom_histogram(binwidth = 1, fill = "purple", color = "black") + 
    ggtitle("Histogram of Crop Yield") + 
    xlab("Crop Yield (tons per hectare)")
)

# Combine histograms into a single grid layout
grid.arrange(
  histograms$Rainfall,
  histograms$Temperature,
  histograms$Sunlight,
  histograms$Fertilizer,
  histograms$Crop_Yield,
  ncol = 3, # Number of columns in the grid
  nrow = 2  # Number of rows in the grid
)

# Combine histograms into a single grid layout
combined_plot <- (histograms$Rainfall | histograms$Temperature) /
  (histograms$Sunlight | histograms$Fertilizer) /
  histograms$Crop_Yield

# Print the combined plot
print(combined_plot)


# b. Check for normality using skewness and distribution shape
normality_checks <- list(
  Rainfall_skewness = skewness(data$Rainfall, na.rm = TRUE),
  Temperature_skewness = skewness(data$Temperature, na.rm = TRUE),
  Sunlight_skewness = skewness(data$Sunlight, na.rm = TRUE),
  Fertilizer_skewness = skewness(data$Fertilizer, na.rm = TRUE),
  Crop_Yield_skewness = skewness(data$Crop_Yield, na.rm = TRUE)
)
normality_checks

# Shapiro-Wilk test for normality
normality_tests <- list(
  Rainfall_normality = shapiro.test(data$Rainfall),
  Temperature_normality = shapiro.test(data$Temperature),
  Sunlight_normality = shapiro.test(data$Sunlight),
  Fertilizer_normality = shapiro.test(data$Fertilizer),
  Crop_Yield_normality = shapiro.test(data$Crop_Yield)
)

# Display the p-values from the Shapiro-Wilk test
lapply(normality_tests, function(test) test$p.value)


# Question 2: Comparing Groups Using Box Plots
# a. Box plots to compare Crop Yield across Soil Types
boxplot_crop_yield <- ggplot(data, aes(x = Soil_Type, y = Crop_Yield, fill = Soil_Type)) +
  geom_boxplot() + ggtitle("Crop Yield by Soil Type") + xlab("Soil Type") + ylab("Crop Yield")

print(boxplot_crop_yield)

# b. Check for significant differences in median Crop Yield
# Perform the Kruskal-Wallis test to check for significant differences in median Crop Yield
kruskal_test_result <- kruskal.test(Crop_Yield ~ Soil_Type, data = data)

# Display the results of the Kruskal-Wallis test
print(kruskal_test_result)


# Question 3: Analyzing Relationships Between Variables
# a. Scatter plots to examine relationships
scatter_plots <- list(
  Temp_vs_Growth = ggplot(data, aes(x = Temperature, y = Crop_Growth)) + geom_point(color = "blue") + ggtitle("Temperature vs Crop Growth Rate"),
  Fertilizer_vs_Yield = ggplot(data, aes(x = Fertilizer, y = Crop_Yield)) + geom_point(color = "green") + ggtitle("Fertilizer Usage vs Crop Yield")
)

# Plot scatter plots
lapply(scatter_plots, print)

# b. Describe relationship and identify clusters
# Can be done manually after viewing scatter plots

# Question 4: Correlation Analysis
# a. Correlation heatmap of numerical variables

cor_matrix <- cor(data %>% dplyr::select(-Soil_Type), use = "complete.obs")
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7, tl.srt = 45)

# Question 5: Multivariate Outlier Detection
# a. Scatter plot matrix to identify multivariate outliers
pair_plot <- ggpairs(data %>% dplyr::select(-Soil_Type), aes(color = data$Soil_Type))

print(pair_plot)

scatter_yield_vs_rainfall <- ggplot(data, aes(x = Rainfall, y = Crop_Yield)) +
  geom_point(color = "blue") +
  ggtitle("Crop Yield vs Rainfall") +
  xlab("Rainfall (mm)") +
  ylab("Crop Yield (tons per hectare)")

scatter_yield_vs_temperature <- ggplot(data, aes(x = Temperature, y = Crop_Yield)) +
  geom_point(color = "red") +
  ggtitle("Crop Yield vs Temperature") +
  xlab("Temperature (°C)") +
  ylab("Crop Yield (tons per hectare)")

scatter_yield_vs_sunlight <- ggplot(data, aes(x = Sunlight, y = Crop_Yield)) +
  geom_point(color = "green") +
  ggtitle("Crop Yield vs Sunlight") +
  xlab("Sunlight (hours per day)") +
  ylab("Crop Yield (tons per hectare)")

scatter_yield_vs_fertilizer <- ggplot(data, aes(x = Fertilizer, y = Crop_Yield)) +
  geom_point(color = "purple") +
  ggtitle("Crop Yield vs Fertilizer") +
  xlab("Fertilizer (kg per hectare)") +
  ylab("Crop Yield (tons per hectare)")

scatter_growth_vs_rainfall <- ggplot(data, aes(x = Rainfall, y = Crop_Growth)) +
  geom_point(color = "cyan") +
  ggtitle("Crop Growth vs Rainfall") +
  xlab("Rainfall (mm)") +
  ylab("Crop Growth (cm per day)")

scatter_growth_vs_temperature <- ggplot(data, aes(x = Temperature, y = Crop_Growth)) +
  geom_point(color = "orange") +
  ggtitle("Crop Growth vs Temperature") +
  xlab("Temperature (°C)") +
  ylab("Crop Growth (cm per day)")

scatter_growth_vs_sunlight <- ggplot(data, aes(x = Sunlight, y = Crop_Growth)) +
  geom_point(color = "pink") +
  ggtitle("Crop Growth vs Sunlight") +
  xlab("Sunlight (hours per day)") +
  ylab("Crop Growth (cm per day)")

scatter_growth_vs_fertilizer <- ggplot(data, aes(x = Fertilizer, y = Crop_Growth)) +
  geom_point(color = "brown") +
  ggtitle("Crop Growth vs Fertilizer") +
  xlab("Fertilizer (kg per hectare)") +
  ylab("Crop Growth (cm per day)")

scatter_growth_vs_yield <- ggplot(data, aes(x = Crop_Yield, y = Crop_Growth)) +
  geom_point(color = "darkblue") +
  ggtitle("Crop Growth vs Crop Yield") +
  xlab("Crop Yield (tons per hectare)") +
  ylab("Crop Growth (cm per day)")

# Combine all scatter plots into a 3x3 grid
(scatter_yield_vs_rainfall | scatter_yield_vs_temperature | scatter_yield_vs_sunlight) / 
  (scatter_yield_vs_fertilizer | scatter_growth_vs_rainfall | scatter_growth_vs_temperature) / 
  (scatter_growth_vs_sunlight | scatter_growth_vs_fertilizer | scatter_growth_vs_yield)


# Question 6: Graphical Summary for Presentation
# a. Summary visualization combining multiple plots
# Combine histograms into a single grid layout
combined_plot <- (histograms$Rainfall | histograms$Temperature) /
  (histograms$Sunlight | histograms$Fertilizer) /
  histograms$Crop_Yield

# Print the combined plot
print(combined_plot)

print(boxplot_crop_yield)

print(pair_plot)
