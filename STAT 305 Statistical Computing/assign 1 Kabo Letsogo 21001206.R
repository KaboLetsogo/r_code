#1. Import the Choc data on blackboard and explore the data using appropriate
#descriptive statistics. What insights can you get from the data?
library(readxl)
library(dplyr)
library(ggplot2)
Choc <- read_excel("STAT 305 Statistical Computing/Choc-data.xlsx")
Choc$`Sales Person`<-as.factor(Choc$`Sales Person`)
Choc$Country<-as.factor(Choc$Country)
Choc$Product<-as.factor(Choc$Product)
str(Choc)
head(Choc,10)



desc_stats1 <- Choc%>%group_by(`Sales Person`)%>%
  summarise('Av. Amt $' =  mean(Amount),
            'Min Amt $' =  min(Amount),
            'IQR Amt $' =  IQR(Amount),
            'Max Amt $' =  max(Amount),
            'Av. Boxes Shipped' = mean(`Boxes Shipped`),
            'Min Boxes Shipped' =  min(`Boxes Shipped`),
            'IQR. Boxes Shipped' = IQR(`Boxes Shipped`),
            'Max Boxes Shipped' =  max(`Boxes Shipped`))

desc_stats2 <- Choc%>%group_by(Product)%>%
  summarise('Total Amt $' =  sum(Amount),
            'Av. Amt $' =  mean(Amount),
            'IQR Amt $' =  IQR(Amount),
            'Std. Dev Amt $' =  sd(Amount),
            'Median Boxes Shipped' = median(`Boxes Shipped`),
            'IQR. Boxes Shipped' = IQR(`Boxes Shipped`),
            'Total Boxes Shipped' =  sum(`Boxes Shipped`))

desc_stats1
desc_stats2

#From the descriptive statistics we get a rough idea of the performance of each
# product  as well as each sales person 

#From the graph we can also get insight into the sales of products

#  2. Identify any two business questions you may get from the insights of the data.

#how do we reallocate sales people to countries they sell most in?
#which products can we pre stock in which countries as they are sold most there
#does sales person have an effect on amount sold of product X

#3. What statistical methodologies maybe needed to achieve your identified
#business questions?

#A two way analysis of variance could be used to find out if sales person has an 
# effect on product . A product is chosen . it is the independent variable and then
# the treatments are the different sales people. We can use country as a blocking 
# factor incase some sales people are just good in some countries and bad in others

#  4. Select only those boxes with shipping amount less than USD1000.00 and call
#them LOW.COST.

Choc%>%filter(Amount < 1000 )->LOW.COST
head(LOW.COST,5)
#5. Which product is commonly shipped in the low-cost category goods?

LOW.COST%>%group_by(Product)%>%
  summarise('Total Shipped'=sum(`Boxes Shipped`))%>%
  slice(which.max(`Total Shipped`))

#  6. Calculate the average number of boxes peanuts butter cubes by country.
#Which country had the highest number of peanut butter boxes shipped?

Choc%>%filter(Product =='Peanut Butter Cubes')%>%
  group_by(Country)%>%summarise('Av. PBC by Cnty' = mean(`Boxes Shipped`))

#Which country had the highest number of peanut butter boxes
Choc%>%filter(Product =='Peanut Butter Cubes')%>%
  group_by(Country)%>%summarise('Total PBC Boxes' = sum(`Boxes Shipped`))%>%  
  slice(which.max(`Total PBC Boxes`))

#  7. Create a new data frame from the LOW.COST data which excludes date, and
#salesperson columns.

LOW.COST%>%select(-c(`Sales Person`,Date)) -> LOW.COST2
head(LOW.COST2,5)
#8. Create a new column called APB from the amount column divided by boxes
#shipped column. Describe the statistics of this new column. What can you
#conclude?

Choc%>%mutate('APB' = Amount/`Boxes Shipped`)

#
