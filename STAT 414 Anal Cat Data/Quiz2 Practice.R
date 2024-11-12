library(readr)
library(tidyverse)
#install.packages("lmtest")
library(lmtest)
placekick_BW <- read_csv("STAT 414 Anal Cat Data/placekick.BW.csv")
str(placekick_BW)

placekick_BW <- placekick_BW %>%
  mutate(
    Weather = factor(Weather),
    Temperature = factor(Temperature),
    Pressure = factor(Pressure),
    Grass = factor(Grass),
    Wind15 = factor(Wind15),
    Ice = factor(Ice),
    Good = factor(Good, levels = c("N", "Y"))  # Set "N" as the reference for success
  )

model <- glm(Good ~ Distance + Weather + Wind15 + Temperature + Grass + Pressure + Ice,
             family = binomial, data = placekick_BW)

summary(model)

##### 1B

#placekick_BW$Weather <- relevel(placekick_BW$Weather, ref = "Sun")

#####  1C

# Fit the full logistic regression model
model_full <- glm(Good ~ Distance + Weather + Wind15 + Temperature + Grass + Pressure + Ice,
                  family = binomial, data = placekick_BW)

# List of variable names to drop one at a time
variables <- c("Distance", "Weather", "Wind15", "Temperature", "Grass", "Pressure", "Ice")

# Function to fit reduced models and perform LRT
perform_lrtest <- function(full_model, data, drop_var) {
  # Create formula for reduced model by dropping one variable
  formula <- as.formula(paste("Good ~", paste(setdiff(variables, drop_var), collapse = " + ")))
  
  # Fit reduced model
  model_reduced <- glm(formula, family = binomial, data = data)
  
  # Perform LRT between full and reduced models
  lrtest(full_model, model_reduced)
}

# Iterate over each variable and perform LRT
for (var in variables) {
  cat("\nLRT for dropping", var, "from the model:\n")
  print(perform_lrtest(model_full, placekick_BW, var))
}

########## 1D

# Get the coefficient and confidence interval for Distance
coef_distance <- coef(model_full)["Distance"]
confint_distance <- confint(model_full, "Distance")

# Calculate the odds ratio for Distance
odds_ratio <- exp(coef_distance)

# Calculate the 95% confidence interval for the odds ratio
confint_odds_ratio_distance <- exp(confint_distance)

# Output the odds ratio and confidence interval
odds_ratio
confint_odds_ratio_distance

###### 1E

placekick_BW$Weather <- relevel(placekick_BW$Weather, ref = "Clouds")

# Get the coefficients for Weather
coef_weather <- coef(model_full)[grep("Weather", names(coef(model_full)))]

# Calculate the odds ratios for Weather levels (compared to "Sun")
odds_ratios_weather <- exp(coef_weather)

# Calculate 95% confidence intervals for Weather odds ratios
confint_weather <- exp(confint(model_full)[grep("Weather", rownames(confint(model_full))), ])

# Output the odds ratios and confidence intervals
odds_ratios_weather
confint_weather

placekick_BW$Weather <- relevel(placekick_BW$Weather, ref = "Sun")

# Get the coefficients for Weather
coef_weather <- coef(model_full)[grep("Weather", names(coef(model_full)))]

# Calculate the odds ratios for Weather levels (compared to "Sun")
odds_ratios_weather <- exp(coef_weather)

# Calculate 95% confidence intervals for Weather odds ratios
confint_weather <- exp(confint(model_full)[grep("Weather", rownames(confint(model_full))), ])

# Output the odds ratios and confidence intervals
odds_ratios_weather
confint_weather

placekick_BW$Weather <- relevel(placekick_BW$Weather, ref = "Clouds")


####### 1F


# Number of comparisons
num_comparisons <- 3
# Extract the p-values for the Weather coefficients
p_values <- summary(model_full)$coefficients[grep("Weather", rownames(summary(model_full)$coefficients)), 4]

# Apply the Bonferroni correction
adjusted_p_values <- p.adjust(p_values, method = "bonferroni", n = num_comparisons)

# Output the original and adjusted p-values
p_values
adjusted_p_values
placekick_BW$Weather <- relevel(placekick_BW$Weather, ref = "Sun")

# Extract the p-values for the Weather coefficients
model_full2 <- glm(Good ~ Distance + Weather + Wind15 + Temperature + Grass + Pressure + Ice,
                  family = binomial, data = placekick_BW)
p_values <- summary(model_full2)$coefficients[grep("Weather", rownames(summary(model_full2)$coefficients)), 4]

# Apply the Bonferroni correction
adjusted_p_values <- p.adjust(p_values, method = "bonferroni", n = num_comparisons)

# Output the original and adjusted p-values
p_values
adjusted_p_values
placekick_BW$Weather <- relevel(placekick_BW$Weather, ref = "Clouds")


summary(model_full)

############ 1G

# Create a new data frame with the values for the specific placekick
new_placekick <- data.frame(
  Distance = 49,
  Weather = "Sun",
  Wind15 = factor(0, levels = levels(placekick_BW$Wind15)),
  Temperature = "Nice",
  Grass = factor(1, levels = levels(placekick_BW$Grass)),
  Pressure = "Y",
  Ice = factor(1, levels = levels(placekick_BW$Ice))
)

# Predict the probability of success
predicted_prob <- predict(model_full, newdata = new_placekick, type = "response")

# Output the predicted probability
predicted_prob

# Get the logit prediction with standard error
pred_logit <- predict(model_full, newdata = new_placekick, type = "link", se.fit = TRUE)

# Calculate the 95% confidence interval on the logit scale
ci_logit <- pred_logit$fit + c(-1.96, 1.96) * pred_logit$se.fit

# Convert the confidence interval back to the probability scale
ci_prob <- exp(ci_logit) / (1 + exp(ci_logit))

# Output the confidence interval
ci_prob


######   1H

# Extract the coefficient and p-value for Ice1 from the model summary
coef_ice <- coef(summary(model_full))["Ice1", ]
odds_ratio_ice <- exp(coef_ice["Estimate"])
p_value_ice <- coef_ice["Pr(>|z|)"]
confint_ice <- exp(confint(model_full)["Ice1", ])

# Output the results
list(
  "Coefficient for Ice1" = coef_ice["Estimate"],
  "Odds Ratio for Ice1" = odds_ratio_ice,
  "P-value for Ice1" = p_value_ice,
  "95% Confidence Interval for Odds Ratio" = confint_ice
)

#The coefficient for Ice1 is -0.8761, which means that when a timeout is called before the kick (i.e., icing the kicker), the log odds of a successful kick decrease by 0.876
#The odds ratio is 0.4164. This means that the odds of success with icing are about 41.6% of the odds of success without icing, indicating a decrease in the likelihood of success when the kicker is iced
#



###################################################################################

#Q2

#########################################################################

###2A

# Model without interaction
model_no_interaction <- glm(Good ~ Distance + Wind15, family = binomial, data = placekick_BW)
model_no_interaction
# Model with interaction
model_with_interaction <- glm(Good ~ Distance * Wind15, family = binomial, data = placekick_BW)
model_with_interaction
###2B

# Create a data frame for prediction
distance_values <- seq(min(placekick_BW$Distance), max(placekick_BW$Distance), by = 1)
distance_values
wind_levels <- unique(placekick_BW$Wind15)
wind_levels
# Generate predictions for each combination of Distance and Wind15
prediction_data <- expand.grid(Distance = distance_values, Wind15 = wind_levels)
prediction_data$predicted_prob <- predict(model_with_interaction, newdata = prediction_data, type = "response")

# Plot the interaction effect
ggplot(prediction_data, aes(x = Distance, y = predicted_prob, color = as.factor(Wind15))) +
  geom_line() +
  labs(title = "Effect of Distance and Wind15 Interaction on Success Probability",
       x = "Distance (yards)", y = "Predicted Probability of Success",
       color = "Wind15") +
  theme_minimal()

#Notice that the gap between the red and blue lines increases as distance increases. This suggests that the negative impact of high wind speed becomes more pronounced as the distance increases.

#### 2C


#### 2D

lrt_result <- lrtest(model_no_interaction, model_with_interaction)

# Output the LRT result
lrt_result

#Since the log-likelihood of Model 2 is very close to that of Model 1, the interaction term is not providing much improvement in the model fit.
#The p-value of 0.7135 indicates that there is no evidence to suggest that the interaction between Distance and Wind15 is important for predicting the probability of a successful kick.