require(datasets)
data(mtcars)
library(ggplot2)
g1head(mtcars)
#“Is an automatic or manual transmission better for MPG”
# confounders: 

fitall <- lm(mpg ~ ., data=mtcars)
summary(fitall)

# from p-data, predictors are correlated

fit1 <- lm(mpg ~ am , data=mtcars)
summary(fit1)  # p-value 0.00285. Reject
e1 <- resid(fit1)

fit2 <- lm(mpg ~ cyl,data=mtcars)
summary(fit2)  # good model, 
#e2 <- resid(fit2)

fit3 <- lm(mpg ~ cyl + am,data=mtcars)
summary(fit3)  # bad am p-value 
#value of the relationship between mpg and cyl remains
#largely unchanged when considering am
#check the residual variation. Should be almost no resid variability
#after accounting for am

fit4 <- lm(mpg ~ cyl + wt,data=mtcars)
summary(fit4)  # wt and cyl stronger, am bad


fit5 <- lm(mpg ~ cyl  +wt,data=mtcars)
summary(fit5)  # 

anova(fit1,fit2, fit3, fit4, fit5)
library(car)
vif(fit3) #check for variance inflation

#havalues shows no outliers/influencersc
#Modeling multivariate relationships is difficult.
#Play around with simulations to see how the inclusion or exclustion of another variable can change
#analyses

plot(predict(fit3),resid(fit3 ))
