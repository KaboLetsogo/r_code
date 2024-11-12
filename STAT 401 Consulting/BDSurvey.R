library(readr)
library(tidyverse)
#install.packages("haven")
library(haven)

BDS2017_Data <- read_sav("STAT 401 Consulting/BDS2017_Data.sav")
#Sampled Population
nrow(BDS2017_Data)

#Find number of males/females

BDS2017_Data %>%
  group_by(p05) %>%
  summarise(count = n(),
            percentage =  (count / nrow(BDS2017_Data)) * 100
            )

#No. of Non Batswana

BDS2017_Data %>%
  group_by(p09) %>%
  summarise(count = n(),
            percentage =  (count / nrow(BDS2017_Data)) * 100
  )

BDS2017_Data %>%
  summarise(count = sum(p09 != 1),
            percentage =  (count / nrow(BDS2017_Data)) * 100
  )

#Age distributions

BDS2017_Data <- BDS2017_Data %>%
  mutate(age_group1 = case_when(
    p06 >= 0 & p06 <= 4 ~ "0-4",
    p06 >= 5 & p06 <= 14 ~ "5-14",
    p06 >= 15 & p06 <= 64 ~ "15-64",
    p06 > 65 ~ "65+"
  ))

BDS2017_Data <- BDS2017_Data %>%
  mutate(age_group2 = case_when(
    p06 >= 0 & p06 <= 14 ~ "0-14",
    p06 >= 15 & p06 <= 49 ~ "15-49",
    p06 > 49 ~ "49+"
  ))

BDS2017_Data %>%
  group_by(age_group1) %>%
  summarise(
    count = n(),
    percentage = (count / nrow(BDS2017_Data)) * 100
  )

BDS2017_Data %>%
  group_by(age_group2) %>%
  summarise(
    count = n(),
    percentage = (count / nrow(BDS2017_Data)) * 100
  )

# Percentage of Females aged 15-49

BDS2017_Data %>%
  filter(p05 == 2) %>%  # Filter for females first
  summarise(
    total_females = n(),  # Total number of females
    count_females_15_49 = sum(p06 >= 15 & p06 <= 49),  # Count females aged 15-49
    percentage_females_15_49 = (count_females_15_49 / total_females) * 100  # Calculate the percentage
  )

# Calculate Dependency Ratio
 BDS2017_Data %>%
  summarise(
    dependents = sum(p06 >= 0 & p06 <= 14 | p06 > 65),
    working_age = sum(p06 >= 15 & p06 <= 64),
    dependency_ratio = (dependents / working_age) * 100
  )

 # Calculate Child-Women Ratio
 BDS2017_Data %>%
   summarise(
     children_0_4 = sum(p06 >= 0 & p06 <= 4),
     women_15_49 = sum(p05 == 2 & p06 >= 15 & p06 <= 49),
     ratio = (children_0_4 / women_15_49) * 1000
   )
 
 # Calculate Sex Ratio
 BDS2017_Data %>%
   summarise(
     males = sum(p05 == 1),
     females = sum(p05 == 2),
     sex_ratio = (males / females) * 100
   )
 
 # Calculate Percentage Urban
 
 # Calculate Sample Density
 BDS2017_Data %>%
   summarise(
     total_population = n(),
     area_km2 = 581730 ,
     density = total_population / area_km2
   )

 # Calculate Crude Birth Rate
  BDS2017_Data %>%
   summarise(
     total_births = sum(BDS2017_Data$p39f, na.rm = TRUE) + sum(BDS2017_Data$p39m, na.rm = TRUE),
     total_population = n(),
     crude_birth_rate = (total_births / total_population) * 1000
   ) 

  # Calculate Crude Death Rate 
  
  #crude_death_rate <- BDS2017_Data %>%
  #  summarise(
  #    total_deaths = sum(BDS2017_Data$d2 == 1), #d2 is a missing column seemingly
  #    total_population = n(),
  #    crude_death_rate = (total_deaths / total_population) * 1000
  #  )  
  
  # Calculate General Fertility Rate
  BDS2017_Data %>%
    summarise(
      total_births = sum(BDS2017_Data$p39f, na.rm = TRUE) + sum(BDS2017_Data$p39m, na.rm = TRUE),
      women_15_49 = sum(p05 == 2 & p06 >= 15 & p06 <= 49),
      general_fertility_rate = (total_births / women_15_49) * 1000
    )
  
  #
  
  #
  
  #
  
  # Calculate Mean Age
  BDS2017_Data %>%
    group_by(p05) %>%
    summarise(mean_age = mean(p06, na.rm = TRUE))
  
  
  # Calculate Median Age
  BDS2017_Data %>%
    group_by(p05) %>%
    summarise(median_age = median(p06, na.rm = TRUE)) 
  
  
  
  ###############################################################################
  ###############################################################################
  ##################################################################################
  #TABLE 2.5
  #Town development level data is missing
  BDS2017_Data %>%
    group_by(p05) %>%
    summarise(count = n(),
              percentage =  (count / nrow(BDS2017_Data)) * 100,
              Batswana_count = sum(p09 == 1),
              Batswana_percentage = Batswana_count/sum(BDS2017_Data$p09 == 1),
              Non_Batswana_count = sum(p09 != 1),
              Non_Batswana_percentage = Non_Batswana_count/sum(BDS2017_Data$p09 != 1)
    )
  
  
  ###############################################################################
  ###############################################################################
  fonts = 21
  ##################################################################################
  #TABLE 2.7.2
  #Town development level data is missing
  plot_data = BDS2017_Data %>%
    filter(p06 >= 6) %>%
    filter(!is.na(p20)) %>%
    group_by(p20) %>%
    summarise(count = n(),
              percentage =  (count / nrow(BDS2017_Data)) * 100,
    )
  plot_data$p20 <- as.factor(plot_data$p20)
  
  
  Slabels <- attributes(BDS2017_Data$p20)$labels
  
  plot_data$p20 <- factor(plot_data$p20, levels = Slabels, labels = names(Slabels))
  
  
  # Create the bar chart
  ggplot(plot_data, aes(x = p20, y = percentage)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    labs(title = "Percentage Distribution for Ages 6 and Above by school attendance",
         x = "Attendance",
         y = "Percentage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = fonts),
          axis.text.y = element_text(size = fonts),  
          axis.title.x = element_text(size = fonts), 
          axis.title.y = element_text(size = fonts)) 
  ###############################################################################
  ###############################################################################
  ##################################################################################
  #TABLE 2.9
  
 plot_data <- BDS2017_Data %>%
   filter(!is.na(p26)) %>%
    group_by(p26) %>%
    summarise(count = n(),
              percentage = (count / nrow(BDS2017_Data)) * 100)
  
  religion_labels <- attributes(BDS2017_Data$p26)$labels
  
  plot_data$p26 <- factor(plot_data$p26, levels = religion_labels, labels = names(religion_labels))

  ggplot(plot_data, aes(x = p26, y = percentage)) +
    geom_bar(stat = "identity", fill = "red", color = "black") +
    labs(title = "Percentage Distribution of Religions",
         x = "Religion",
         y = "Percentage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = fonts),
  axis.text.y = element_text(size = fonts),  
  axis.title.x = element_text(size = fonts), 
  axis.title.y = element_text(size = fonts)) 
  
  
  ###############################################################################
  ###############################################################################
  ##################################################################################
  #TABLE 2.10
  plot_data <- BDS2017_Data %>%
    filter(!is.na(p27)) %>%
    group_by(p27) %>%
    summarise(count = n(),
              percentage = (count / nrow(BDS2017_Data)) * 100)
  
  Mlabels <- attributes(BDS2017_Data$p27)$labels
  
  plot_data$p27 <- factor(plot_data$p27, levels = Mlabels, labels = names(Mlabels))
  
  ggplot(plot_data, aes(x = p27, y = percentage)) +
    geom_bar(stat = "identity", fill = "green", color = "black") +
    labs(title = "Percentage Distribution of Marital Status",
         x = "Marriage",
         y = "Percentage") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = fonts),
  axis.text.y = element_text(size = fonts),  
  axis.title.x = element_text(size = fonts), 
  axis.title.y = element_text(size = fonts)) 
  
 