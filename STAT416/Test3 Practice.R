library(readr)
library(tidyverse)
library(biotools)
library(MASS)
library(MVN)
data <- read_csv("STAT416/UCI Heart Disease Data set.csv")

str(data)

missing_proportion <- (colSums(is.na(data)) / nrow(data)) * 100
missing_proportion <- round(missing_proportion[missing_proportion > 0],digits=2)
missing_proportion

data <- na.omit(data)

data_num <- data %>% dplyr::select(oldpeak, thalch, chol, trestbps, age)


#mvn_result <- mvn(data = data_num, mvnTest = "mardia", multivariatePlot = "qq")
mardia_test <- mvn(data = data_num, mvnTest = "mardia",showOutliers = T, bc = T, showNewData = T)
#mvn_result
mardia_test

#bc = T doesnt transform the data , it gives you stuff you need in order to do transformation, you need to
#know formula and two special cases for boxCox transform being 0 and 1


mvn(data = data[-303,c(thalch, chol, trestbps)], mvnTest = "mardia",showOutliers = T, bc = T)


mahalanobis_distances <- mahalanobis(data_num, colMeans(data_num, na.rm = TRUE), cov(data_num, use = "complete.obs"))
threshold <- qchisq(0.95, df = ncol(data_num))  # 95% confidence level
outliers <- which(mahalanobis_distances > threshold)
