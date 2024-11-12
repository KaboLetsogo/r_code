library(tidyverse)
library(readxl)
installed.packages("car")
library(car)
CTmax <- read_excel("STAT 305 Statistical Computing/CTmax_data.xlsx")
CTmax$Ramping_Rate <- as.factor(CTmax$Ramping_Rate)
CTmax$Treatment <- as.factor(CTmax$Treatment)
str(CTmax)

summary_stats <- CTmax %>%
  group_by(Treatment, Ramping_Rate) %>%
  summarise(
    Min = min(CTmax, na.rm = TRUE),
    Q1 = quantile(CTmax, 0.25, na.rm = TRUE),
    Median = median(CTmax, na.rm = TRUE),
    Q3 = quantile(CTmax, 0.75, na.rm = TRUE),
    Max = max(CTmax, na.rm = TRUE),
    Mean = mean(CTmax, na.rm = TRUE),
    SD = sd(CTmax, na.rm = TRUE),
    IQR = Q3-Q1,
    Range = Max-Min
  )

summary_stats

ggplot(CTmax, aes(x = interaction(Treatment, Ramping_Rate), y = CTmax, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Distribution of CTmax by Treatment and Ramping Rate",
       x = "Treatment and Ramping Rate Combination",
       y = "CTmax (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 




anova_result <- aov(CTmax ~ Treatment * Ramping_Rate, data = CTmax)
summary(anova_result)

shapiro_test <- shapiro.test(residuals(anova_result))
shapiro_test

levene_Test <- leveneTest(CTmax ~ Treatment * Ramping_Rate, data = CTmax)
levene_Test
#
#

CTmax$CTmax_log <- log(CTmax$CTmax)
anova_result_log <- aov(CTmax_log ~ Treatment * Ramping_Rate, data = CTmax)
shapiro_test_log <- shapiro.test(residuals(anova_result_log))
shapiro_test_log

kruskal_test <- kruskal.test(CTmax ~ Treatment + Ramping_Rate, data = CTmax)

kruskal_test
#
#
#
#

kruskal_test_treatment <- kruskal.test(CTmax ~ Treatment, data = CTmax)
kruskal_test_treatment

kruskal_test_ramping_rate <- kruskal.test(CTmax ~ Ramping_Rate, data = CTmax)
kruskal_test_ramping_rate

leveneTest(CTmax ~ Treatment * Ramping_Rate, data = CTmax)
