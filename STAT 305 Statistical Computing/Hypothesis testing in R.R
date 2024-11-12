##############ONE SAMPLE

library(ggplot2)
data<-c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
t.test(x=data,mu=7725)

###############TWO SAMPLE INDEPENDENT

f<-c(38.9,61.2,73.3,21.8,63.4,64.6,48.4,48.4,48.5)
m<-c(67.8,60,63.4,76,89.4,73.3,67.3,61.3,62.4)
weight<-c(f,m)
gender=rep(c("Female","Male"),each=9)
data3<-data.frame(weight,gender)
t.test(x=f,y=m,alternative="two.sided")
boxplot(x=f)
ggplot(data3,aes(x=weight,group=gender))+geom_boxplot()

####################TWO SAMPLE PAIRED

before<-c(200.1,190.9,192.7,213,241.4,196.9,172.2,185.5,205.2,193.7)
after<-c(392.9,393.2,345.1,393,434,427.9,422,383.9,392.3,352.2)
t.test(x=before,y=after,paired=TRUE)
data4<-data.frame(
   group=rep(c("before","after"),each=10),
   weight=c(before,after)
)

t.test(x=before,y=after,paired=TRUE,alternative="greater",var.equal = FALSE)
t.test(x=before,y=after,paired=TRUE,alternative="less",var.equal = TRUE)

#### Interpretation of infinite confidence interval and p-value = 1?

###Hypothesis testing ANOVA

library(readxl)
Choc_data <- read_excel("STAT 305 Statistical Computing/Choc-data.xlsx")
head(Choc_data)
str(Choc_data)

Choc_data$`Sales Person`<-as.factor(Choc_data$`Sales Person`)
Choc_data$Product<-as.factor(Choc_data$Product)
Choc_data$Country<-as.factor(Choc_data$Country)
str(Choc_data)
library(tidyverse)
ggplot(Choc_data,aes(x=Country,y=Amount,group=Country))+geom_boxplot()

AmC<-aov(Amount~Country,data=Choc_data)
summary(AmC)

AmC2<-aov(`Boxes Shipped`~Country,data=Choc_data)
summary(AmC2)

data("ToothGrowth")
str(ToothGrowth)

AmC3<-aov(len~supp,data=ToothGrowth)
summary(AmC3)

AmC4<-aov(dose~supp,data=ToothGrowth)
summary(AmC4)


data("ToothGrowth")
ToothGrowth$doseF<-as.factor(ToothGrowth$dose)
AmC5<-aov(len~doseF,data=ToothGrowth)
summary(AmC5)
TukeyHSD(AmC5)

AmC6<-aov(len~supp*doseF,data=ToothGrowth)
summary(AmC6)
TukeyHSD(AmC6)



