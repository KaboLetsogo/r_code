library(ggplot2)
#Bar graph represents categorical data while histogram shows continous,
library(readr)
CardioFit <- read_csv("STAT 305 Statistical Computing/CardioGoodFitness.csv")
View(CardioFit)


CardioFit$Product<-as.factor(CardioFit$Product)
CardioFit$Gender<-as.factor(CardioFit$Gender)
CardioFit$MaritalStatus<-as.factor(CardioFit$MaritalStatus)
CardioFit$Age<-as.integer(CardioFit$Age)
CardioFit$Education<-as.integer(CardioFit$Education)
head(CardioFit,10)
str(CardioFit)

ggplot(data=CardioFit,aes(x=Usage,y=Fitness))+geom_point()

ggplot(data=CardioFit,aes(x=Age,y=Miles,color=Product))+geom_point()
#Use Jiter when dealing with integers by a small magnitude 
#so that the integers dont plot to same point
ggplot(data=CardioFit,aes(x=Fitness,y=Usage,color=Gender))+geom_point()

ggplot(data=CardioFit,aes(x=Fitness,y=Usage,color=Gender))
+geom_point(position="jitter")
#
ggplot(data=CardioFit,aes(x=Fitness,y=Usage,color=Gender,shape=Product))+
  geom_jitter(width = 1,height = 1)

CardioFit%>%filter(Gender=="Female")%>%count(Product)

CardioFit%>%filter(Gender=="Female")%>%count(Product)

ggplot(data=CardioFit,aes(x=Age,y=Miles,size = Income))+
  geom_point()

g = ggplot(data=CardioFit,aes(x=Age,y=Miles,size = Income,color=Product))+
  geom_point(alpha=0.5)
plot(g)
g+theme_bw()
#This plots with a linear regression line , method is lm by default
#which is linear regression, SE = FALSE to remove stdDev lines
g+geom_smooth(method = 'lm',SE = FALSE)   

g+theme_bw()
g+theme_classic()+labs(title='Miles coveredby Age',subtitle = 'Miles with Income')
g+theme_bw()+geom_line(size=0.75)

g+scale_x_reverse()
str(CardioFit)
g+facet_grid('Gender')+geom_line(size=0.75)

#EXERCISE
#Income Categorised by Product
#the Y axis of a density function is a probability not frequency 
ggplot(data=CardioFit,aes(x=Income,fill=Product,alpha=0.3))+
  geom_density()+
  labs(title='Income by Product',subtitle = 'CardioFit Data')
#so product associated with poor is TM498 because it is the leftmost peak 
#meaning it has the highest probability of low income earners
