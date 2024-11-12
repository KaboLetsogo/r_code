library(readr)
library(car)
placekick <- read.table("STAT 414 Anal Cat Data/Placekick.csv",
                        header = TRUE , sep = ",")

mod_fit <- glm( formula = good ~ distance ,
                family =binomial ( link = logit ), data = placekick )
#mod_fit
#names(mod_fit)
summary(mod_fit)
#class(mod_fit)

mod_fit2 <- glm( formula = good ~ change + distance ,
                family =binomial ( link = logit ), data = placekick )
#mod_fit2$Coefficients

#round(summary (mod_fit) $coefficients ,4)
#vcov(mod_fit) #Variance covariance matrix
summary(mod_fit2)

mod_fit3 <- glm( formula = good ~ PAT + distance ,
                 family =binomial ( link = logit ), data = placekick )
summary(mod_fit3)

mod_fit4 <- glm( formula = good ~ change + distance + elap30,
                 family =binomial ( link = logit ), data = placekick )
summary(mod_fit4)


logL = function(beta,x,y)
 {
  pi = exp(beta[1] + beta[2] * x)/(1+exp(beta[1] + beta[2] * x))
  sum(y * log(pi) + (1-y) * log(1 - pi))
 }

logL(beta = mod_fit$coefficients, x = placekick$distance, y = placekick$good)
logLik(mod_fit)

anova(mod_fit4, test = "LR")

anova(mod_fit,mod_fit2, test = "Chisq")




