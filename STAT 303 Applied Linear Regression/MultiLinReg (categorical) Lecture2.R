install.packages('car')
library(tidyverse)
library(datarium)
library(caret)
library(leaps)
library(MASS)
library(readxl)
library(corrplot)
library(car)

pzza <- read_excel("PizzaHut.xls")
pzza$Gender <- as.factor(pzza$Gender)
pzza$Purchase <- as.factor(pzza$Purchase)
titanic <- read.csv("Titanic.csv")
str(pzza)
 #CATEGORICAL DATA
mod_pzz <- lm(Price~Gender+Purchase,data=pzza)
summary(mod_pzz)

view(titanic)
str(titanic)

salaries <- data('Salaries')
str(Salaries)

model = lm(salary~.,data=Salaries)
str(Salaries)
summary(model)

mod_log <- glm(Purchase~.,data=pzza,family="binomial")
summary(mod_log)

#Coefficients:
#Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  1.21951    1.04877   1.163  0.24491   
#Gender1      0.03769    0.36511   0.103  0.91778   
#Price       -0.25019    0.09343  -2.678  0.00741 **

#Gender1: the log odds of Purchase by Gender1 (males) Compared to Gender0
# are higher by 0.03769

#Price: for every unit increase in Price there is a decrease 
# in log odds of Purchases by 0.25019

exp(mod_log$coefficients)

#(Intercept)     Gender1       Price 
#3.3855436   1.0384107   0.7786556 


# A deadly trick question can be interpreting outoput of above line
# The above outputs are no longer log odds but odds.
#Now the deadliness is interpreting in percentages
#
#Gender1: Gender1 has a 3.8% higher chance of Purchasing than Gender
#
#Price: A unit increase in price will lead to a 22.2% decrease in the
#chance of a purchase 
#

#Null deviance: 205.58  on 219  degrees of freedom
#Residual deviance: 197.90  on 217  degrees of freedom
#
#If Null>Residual the model is appropriate since a null model is when
#the categories are not considered and so we expect a decrease in
#in error wehn categories are added

mod2 <- glm(Purchase~Price,data=pzza,family = "binomial")
mod2
summary(mod2)


str(titanic)
titanic$PClass <- as.factor(titanic$PClass)
titanic$Sex <- as.factor(titanic$Sex) 
titanic$Survived <- as.factor(titanic$Survived) 
mod_sur <- glm(Survived~PClass+Age+Sex,data=titanic,family="binomial")
mod_sur
summary(mod_sur)
