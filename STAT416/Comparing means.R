# Load necessary library
library(knitr)

# Create the data as a matrix or data frame
effluent_data <- data.frame(
  Sample = 1:11,
  Commercial_Lab_BOD = c(6, 6, 18, 8, 11, 34, 28, 71, 43, 33, 20),
  Commercial_Lab_SS = c(27, 23, 64, 44, 30, 75, 26, 124, 54, 30, 14),
  State_Lab_BOD = c(25, 28, 36, 35, 15, 44, 42, 54, 34, 29, 39),
  State_Lab_SS = c(15, 13, 22, 29, 31, 64, 30, 64, 56, 20, 21)
)

# Print the table using kable
kable(effluent_data, 
      col.names = c("Sample", "Commercial Lab (BOD)", "Commercial Lab (SS)", 
                    "State Lab (BOD)", "State Lab (SS)"), 
      caption = "Effluent Data",
      align = "c")
