HP<-c(245,312,279,308,199,219,405,324,319,255)
X<-c(1400,1600,1700,1875,1100,1550,2350,2450,1425,1700)
df1<-as.data.frame(cbind(X,HP))
plot(X,HP)
cor(X,HP)
cor.test(X,HP)
#Since the confidence interval does not include zero we can reject the null hypothesis
#And can conclude that there is a significant linear relationship between X and HP
reg1<-lm(HP~X,data=df1)
reg1
# HP = 98.2483 + 0.1098X
#lm(formula = HP - X)
#For every unit increase in the size of the house the will be  a 0.1098 increase in
#The price of the house. (change,direction of change , how much of the change)
summary(reg1)
# The null hypothesis of theintercept test is the our model does not have an intercept . i.e = H_0
# The null hypothesis of the X is whether or not X has an effect on Y
# Thw null hypothesis of the F test is that house price is not affected by square feet 


#Residual Analysis
res1 <- reg1$residuals
pred1 <- reg1$fitted.values
hist(res1)
#It is right skewed because using the bar of highest frequency 
#as mean and mode , there are more values on the right than on the left
hist(pred1)
shapiro.test(res1)
#The null hypothesis of the shapiro test is that the values are normally distributed
#So we fail to reject and can conclude res1 is normally distributed.
plot(pred1,res1)

plot(X,res1)

acf(res1)
