library(readr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("haven")
library(haven)
library(ggplot2)
#install.packages("tidyr")
library(tidyr)
install.packages("ResourceSelection")
library(ResourceSelection)

BDS2017_Data1 <- read_sav("STAT 401 Consulting/BDS2017_Data.sav")

BDS2017_Data = BDS2017_Data1 %>%
  filter(p05 == 2) %>%  # Filter for females first
  filter(p06 >= 12 & p06 <= 49) # Filter for femailes aged 15-49

BDS2017_Data <- BDS2017_Data %>%
  mutate(parity_group = case_when(
    p35f == 0 ~ "0",
    p35f >= 1 & p35f <= 2 ~ "1-2",
    p35f >= 3 & p35f <= 4 ~ "3-4",
    p35f >= 5 ~ "5+",
    p35f == 99 ~ "Not stated",
    is.na(p35f) ~ "0"
  ))

BDS2017_Data <- BDS2017_Data %>%
  mutate(education_group = case_when(
    p24a %in% c(31, 32, 33, 34, 35, 36) ~ "Certificate",   # All certificates
    p24a %in% c(41, 42, 43, 44) ~ "Diploma",               # All diplomas
    p24a %in% c(45, 51, 52, 53) ~ "Degree",                # Degrees, postgraduate, other degree
    p24a == 99 ~ "At & Below Form 5",
    is.na(p24a) ~ "At & Below Form 5"
  ))

BDS2017_Data <- BDS2017_Data %>%
  mutate(employment_group = case_when(
    p28 == 1 ~ "Seasonal work: paid",          # Seasonal work: paid
    p28 == 2 ~ "Seasonal work: unpaid",        # Seasonal work: unpaid
    p28 == 3 ~ "Non-seasonal work: paid",      # Non-seasonal work: paid
    p28 == 4 ~ "Non-seasonal work: unpaid",    # Non-seasonal work: unpaid
    p28 %in% c(5, 6, 7, 8, 9, 95) ~ "Unemployed", # Grouping Other categories as Unemployed
    p28 == 99 ~ "Unemployed"                  # Not stated
  ))

BDS2017_Data <- BDS2017_Data %>%
  mutate(religion_group = case_when(
    p26 == 1 ~ "Christianity",
    p26 == 2 ~ "Islam",
    p26 == 3 ~ "Bahai",
    p26 == 4 ~ "Hinduism",
    p26 == 5 ~ "Badimo",
    p26 == 6 ~ "No Religion",
    TRUE ~ "No Religion"  # Combines 95 and 99 into 'Other/Not Stated'
  ))

BDS2017_Data <- BDS2017_Data %>%
  mutate(marital_status_group = case_when(
    p27 == 1 ~ "Married",
    p27 == 2 ~ "Never Married",
    p27 == 3 ~ "Living Together",
    p27 == 4 ~ "Separated",
    p27 == 5 ~ "Divorced",
    p27 == 6 ~ "Widowed",
    TRUE ~ "Never Married"  # Combines 9 into 'Not Stated'
  ))

BDS2017_Data <- BDS2017_Data %>%
  mutate(contraceptive_type_cat = case_when(
    f8 == 1 ~ "Pill",
    f8 == 2 ~ "IUD",
    f8 == 3 ~ "Injection",
    f8 == 4 ~ "Diaphragm/Foam/Jelly",
    f8 == 5 ~ "Male Condom",
    f8 == 6 ~ "Female Condom",
    f8 == 7 ~ "Female Sterilization",
    f8 == 8 ~ "Male Sterilization",
    f8 == 9 ~ "Traditional",
    f8 == 10 ~ "Periodic Abstinence",
    f8 == 11 ~ "Prolonged Abstinence",
    f8 == 12 ~ "Withdrawal",
    f8 == 13 ~ "Norplant",
    f8 == 95 ~ "Other",
    is.na(f8) ~ "None"  # Replacing NA with "None"
  ))

BDS2017_Data <- BDS2017_Data %>%
  mutate(HIV_status = case_when(
    p43a__17 == 1 ~ "Positive",
    p43a__17 == 0 ~ "Negative",
    p43a__17 == 9 ~ "Negative"
  ))

BDS2017_Data <- BDS2017_Data %>%
  mutate(Asthma_status = case_when(
    p43a__9 == 1 ~ "Positive",
    p43a__9 == 0 ~ "Negative",
    p43a__9 == 9 ~ "Negative"
  ))

BDS2017_Data <- BDS2017_Data %>%
  mutate(contraceptive_use = ifelse(f6 == 1, 1, 0))  

#Frequencies

attributes(BDS2017_Data$f8)$labels
round(prop.table(table(BDS2017_Data$f8))*100,digits=2) # Contraceptive Type F8

attributes(BDS2017_Data$f7)$labels
round(prop.table(table(BDS2017_Data$f7))*100,digits=2) # Contraceptive this year
round(prop.table(table(BDS2017_Data$f6))*100,digits=2) # Contraceptive ever before



attributes(BDS2017_Data$p26)$labels
round(prop.table(table(BDS2017_Data$p26))*100,digits=2) # Religion

attributes(BDS2017_Data$p27)$labels
round(prop.table(table(BDS2017_Data$p27))*100,digits=2) # Marital Status

#attributes(BDS2017_Data$p21)$labels # Education 
#table(BDS2017_Data$p21) #Too Verbose imo

#attributes(BDS2017_Data$p24a)$labels # Education
#round(prop.table(table(BDS2017_Data$p24a))*100,digits=2)

# View the percentage distribution of the new grouped variable
round(prop.table(table(BDS2017_Data$education_group)) * 100, digits = 2)

#attributes(BDS2017_Data$p28)$labels # Employment
#round(prop.table(table(BDS2017_Data$p28))*100,digits=2)

# View the percentage distribution of the new grouped variable
round(prop.table(table(BDS2017_Data$employment_group)) * 100, digits = 2)




attributes(BDS2017_Data$p30)$labels # Employment
round(prop.table(table(BDS2017_Data$p30))*100,digits=2)

round(prop.table(table(BDS2017_Data$parity_group))*100,digits=2)#Parity:  numb live infants born by females

round(prop.table(table(BDS2017_Data$p43a__9))*100,digits=2) # Asthma
round(prop.table(table(BDS2017_Data$p43a__17))*100,digits=2) # HIV






#Cross tabs against Contraceptives

# Religion
attributes(BDS2017_Data$f8)$labels
attributes(BDS2017_Data$p26)$labels
round(prop.table(table(BDS2017_Data$f8, BDS2017_Data$p26))*100,digits=2)

# Marital Status
attributes(BDS2017_Data$f8)$labels
attributes(BDS2017_Data$p27)$labels
round(prop.table(table(BDS2017_Data$f8, BDS2017_Data$p27))*100,digits=2)

# Employment
attributes(BDS2017_Data$f8)$labels
round(prop.table(table(BDS2017_Data$f8, BDS2017_Data$employment_group))*100,digits=2)

# Education
attributes(BDS2017_Data$f8)$labels
round(prop.table(table(BDS2017_Data$f8, BDS2017_Data$education_group))*100,digits=2)

# Parity
attributes(BDS2017_Data$f8)$labels
round(prop.table(table(BDS2017_Data$f8, BDS2017_Data$parity_group))*100,digits=2)

## HEALTH
#Asthma
attributes(BDS2017_Data$f8)$labels
round(prop.table(table(BDS2017_Data$f8, BDS2017_Data$p43a__9))*100,digits=2)
#HIV 
attributes(BDS2017_Data$f8)$labels
round(prop.table(table(BDS2017_Data$f8, BDS2017_Data$p43a__17))*100,digits=2)


# Visualisations



# Stacked bar chart: Contraceptive type by education level with counts
ggplot(BDS2017_Data, aes(x = contraceptive_type_cat, fill = education_group)) +
  geom_bar(position = "stack") +  # "stack" to show counts
  labs(x = "Contraceptive Type", y = "Count", fill = "Education Level") +
  theme_minimal() +
  ggtitle("Contraceptive Type by Education Level (Counts)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stacked bar chart: Contraceptive type by employment status with counts
ggplot(BDS2017_Data, aes(x = contraceptive_type_cat, fill = employment_group)) +
  geom_bar(position = "stack") +  # "stack" to show counts
  labs(x = "Contraceptive Type", y = "Count", fill = "Employment Status") +
  theme_minimal() +
  ggtitle("Contraceptive Type by Employment Status (Counts)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

contraceptive_data <- BDS2017_Data %>%
  filter(!is.na(contraceptive_type_cat)) %>%
  count(contraceptive_type_cat) %>%
  mutate(perc = n / sum(n) * 100)

# Pie chart of contraceptive types used
ggplot(contraceptive_data, aes(x = "", y = perc, fill = contraceptive_type_cat)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(fill = "Contraceptive Type", y = "Percentage", x = "", title = "Distribution of Contraceptive Types Used") +
  theme_void()  # Clean theme for a pie chart

heatmap_data <- BDS2017_Data %>%
  filter(!is.na(contraceptive_type_cat), !is.na(marital_status_group)) %>%
  count(contraceptive_type_cat, marital_status_group) %>%
  mutate(perc = n / sum(n) * 100)

# Heatmap of contraceptive type by marital status
ggplot(heatmap_data, aes(x = contraceptive_type_cat, y = marital_status_group, fill = perc)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightgreen", high = "purple") +
  labs(x = "Contraceptive Type", y = "Marital Status", fill = "Percentage", title = "Heatmap of Contraceptive Type by Marital Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a contingency table for contraceptive type and religion
heatmap_data <- BDS2017_Data %>%
  filter(!is.na(contraceptive_type_cat), !is.na(religion_group)) %>%
  count(contraceptive_type_cat, religion_group) %>%
  mutate(perc = n / sum(n) * 100)

# Heatmap of contraceptive type by religion
ggplot(heatmap_data, aes(x = contraceptive_type_cat, y = religion_group, fill = perc)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(x = "Contraceptive Type", y = "Religion", fill = "Percentage", title = "Heatmap of Contraceptive Type by Religion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



####################################################################
#Chapter 5 Analysis
###################################################################



### CHI SQUARED Tests 

#Religion
chisq.test(table(BDS2017_Data$f6, BDS2017_Data$p26))
#
#MaritaL Status
chisq.test(table(BDS2017_Data$f6, BDS2017_Data$p27))
#
#Parity
chisq.test(table(BDS2017_Data$f6, BDS2017_Data$parity_group))
#
#Employment
chisq.test(table(BDS2017_Data$f6, BDS2017_Data$employment_group))
#
#Education
chisq.test(table(BDS2017_Data$f6, BDS2017_Data$education_group))
#
#Health
chisq.test(table(BDS2017_Data$f6, BDS2017_Data$p43a__17))#Asthma
chisq.test(table(BDS2017_Data$f6, BDS2017_Data$p43a__9))#HIV

###Logistic Regression

# Univariate models (each variable separately)

# 1. Religion
logit_model_religion <- glm(contraceptive_use ~ religion_group, data = BDS2017_Data, family = binomial)
summary(logit_model_religion)

# 2. Marital Status
logit_model_marital_status <- glm(contraceptive_use ~ marital_status_group, data = BDS2017_Data, family = binomial)
summary(logit_model_marital_status)

# 3. Education
logit_model_education <- glm(contraceptive_use ~ education_group, data = BDS2017_Data, family = binomial)
summary(logit_model_education)

# 4. Employment
logit_model_employment <- glm(contraceptive_use ~ employment_group, data = BDS2017_Data, family = binomial)
summary(logit_model_employment)

# 5. Parity (Number of live births)
logit_model_parity <- glm(contraceptive_use ~ parity_group, data = BDS2017_Data, family = binomial)
summary(logit_model_parity)

# 6. HIV Status
logit_model_HIV <- glm(contraceptive_use ~ HIV_status, data = BDS2017_Data, family = binomial)
summary(logit_model_HIV)

# 7. Asthma Status
logit_model_asthma <- glm(contraceptive_use ~ Asthma_status, data = BDS2017_Data, family = binomial)
summary(logit_model_asthma)

##Multivar Logistic Regression

logit_model <- glm(contraceptive_use ~ religion_group + marital_status_group + education_group + employment_group +
                     parity_group + HIV_status  + Asthma_status , data = BDS2017_Data,
                   family = binomial)

summary(logit_model)

# odds ratios

# Calculate odds ratios for univariate models
odds_religion <- exp(coef(logit_model_religion))
odds_marital_status <- exp(coef(logit_model_marital_status))
odds_education <- exp(coef(logit_model_education))
odds_employment <- exp(coef(logit_model_employment))
odds_parity <- exp(coef(logit_model_parity))
odds_HIV <- exp(coef(logit_model_HIV))
odds_asthma <- exp(coef(logit_model_asthma))

# Calculate odds ratios for the multivariate model
odds_multivar <- exp(coef(logit_model))

# Display odds ratios
list(
  Religion_Odds = odds_religion,
  Marital_Status_Odds = odds_marital_status,
  Education_Odds = odds_education,
  Employment_Odds = odds_employment,
  Parity_Odds = odds_parity,
  HIV_Odds = odds_HIV,
  Asthma_Odds = odds_asthma,
  Multivariate_Odds = odds_multivar
)


# Univariate Models
aic_religion <- AIC(logit_model_religion)
aic_marital_status <- AIC(logit_model_marital_status)
aic_education <- AIC(logit_model_education)
aic_employment <- AIC(logit_model_employment)
aic_parity <- AIC(logit_model_parity)
aic_HIV <- AIC(logit_model_HIV)
aic_asthma <- AIC(logit_model_asthma)

# Multivariate Model
aic_multivar <- AIC(logit_model)

# Print AIC values
cat("AIC (Religion):", aic_religion, "\n")
cat("AIC (Marital Status):", aic_marital_status, "\n")
cat("AIC (Education):", aic_education, "\n")
cat("AIC (Employment):", aic_employment, "\n")
cat("AIC (Parity):", aic_parity, "\n")
cat("AIC (HIV):", aic_HIV, "\n")
cat("AIC (Asthma):", aic_asthma, "\n")
cat("AIC (Multivariate):", aic_multivar, "\n")

# Deviance - Multivariate Model
cat("Null deviance:", logit_model$null.deviance, "\n")
cat("Residual deviance:", logit_model$deviance, "\n")

# Hosmer-Lemeshow Test
hoslem_test <- hoslem.test(BDS2017_Data$contraceptive_use, fitted(logit_model), g=10)
print(hoslem_test)


# Remove rows with missing values in relevant columns before the Hosmer-Lemeshow test
BDS2017_Data_clean <- BDS2017_Data %>% 
  filter(!is.na(contraceptive_use), 
         !is.na(religion_group), 
         !is.na(marital_status_group), 
         !is.na(education_group), 
         !is.na(employment_group), 
         !is.na(parity_group), 
         !is.na(HIV_status), 
         !is.na(Asthma_status))

# Refit the multivariate model with the cleaned data
logit_model_clean <- glm(contraceptive_use ~ religion_group + marital_status_group + 
                           education_group + employment_group + parity_group + HIV_status + 
                           Asthma_status, data = BDS2017_Data_clean, family = binomial)

hoslem_test <- hoslem.test(BDS2017_Data_clean$contraceptive_use, fitted(logit_model_clean), g = 10)
print(hoslem_test)
