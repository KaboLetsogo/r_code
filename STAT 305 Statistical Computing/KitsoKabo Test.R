library(ggplot2)
library(dplyr)
library(readr)
CardioFit <- read_csv("CardioGoodFitness (1).csv")
str(CardioFit)

CardioFit$Product<-as.factor(CardioFit$Product)
CardioFit$Gender<-as.factor(CardioFit$Gender)
CardioFit$MaritalStatus<-as.factor(CardioFit$MaritalStatus)
CardioFit$Age<-as.integer(CardioFit$Age)
CardioFit$Education<-as.integer(CardioFit$Education)
CardioFit$Fitness<-as.integer(CardioFit$Fitness)
str(CardioFit)
median(CardioFit$Age)

ggplot(data=CardioFit,aes(x=Education,y=Fitness,color=Product,size=Income))+
  geom_point(position="jitter")+facet_grid('Gender')

P20.df <- CardioFit%>%filter(MaritalStatus=='Partnered',Income>=20000)
P20.df

mean(P20.df$Income)



set.seed(341)

