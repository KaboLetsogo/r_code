install.packages("tidyverse")
install.packages("datarium")
install.packages("caret")
install.packages("leaps")
install.packages("MASS")
install.packages("readxl")
install.packages("corrplot")

library(tidyverse)
library(datarium)
library(caret)
library(leaps)
library(MASS)
library(readxl)
library(corrplot)

pzza <- read_excel("PizzaHut.xls")
pzza$Gender <- as.factor(pzza$Gender)
pzza$Purchase <- as.factor(pzza$Purchase)

data("marketing",package="datarium") #marketing data
head(marketing,4)

cor(marketing$youtube,marketing$sales)
cor(marketing$facebook,marketing$sales)
cor(marketing$newspaper,marketing$sales)

#correlation coeffecient matrix
cor1 = cor(marketing,method="pearson")
cor1

model <- lm(sales~youtube+facebook+newspaper,data = marketing)
summary(model)
#Confidence Interval
confint(model)

model2 <- lm(sales~youtube+facebook,data = marketing)
summary(model2)

confint(model2)
#Stepwise regression in "both" directions forward and backward
step.model <- stepAIC(model,direction = "both", trace = FALSE)
summary(step.model)

fanova <- anova(model)
sanova <- anova(step.model)

fanova
sanova
#Stepwise regression in "backward" 
b.model <- stepAIC(model,direction = "backward", trace = FALSE)
summary(b.model)

f.model <- stepAIC(model,direction = "forward", trace = FALSE)
summary(f.model)


#CATEGORICAL DATA
mod_pzz <- lm(Price~Gender+Purchase,data=pzza)
summary(mod_pzz)

#Gender1 is representing where gender=1 and the Estimate value of -0.1279
#shows us that there is a decrease in price when "category 1" compared to
# category 0

#Inherently each coeffecient is being compared to its category zero where 
#positive number refers to an increase and negative number refers to a 
#decrease




