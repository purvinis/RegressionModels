require(datasets)
data(mtcars)
library(ggplot2)
require(ggplot2)
install.packages("GGally")
require(GGally)
g1head(mtcars)
#“Is an automatic or manual transmission better for MPG”
# confounders: 

fitall <- lm(mpg ~ ., data=mtcars)
summary(fitall)

g1 <- ggpairs(mtcars,lower = list(continuous = wrap("points")),ggplot2::aes(colour=factor(am)))
g1
g2 <- ggcorr(mtcars, method = c("everything","spearman"))
g2
# Scatterplots of each pair of numeric variable are drawn on the left part 
#of the figure. Pearson correlation is displayed on the right. 
#Variable distribution is available on the diagonal.

fit1 <- lm(mpg ~ am , data=mtcars)
summary(fit1)  # p-value 0.00285. Reject
e1 <- resid(fit1)

fit2 <- lm(mpg ~ wt + cyl + disp +vs ,data=mtcars)
summary(fit2)  # good model, 
#e2 <- resid(fit2)

fit3 <- lm(mpg[am==1] ~ cyl[am ==1] + wt[am ==1],data=mtcars)
summary(fit3)  # bad am p-value 
#value of the relationship between mpg and cyl remains
#largely unchanged when considering am
#check the residual variation. Should be almost no resid variability
#after accounting for am

fit4 <- lm(mpg ~ am + cyl ,data=mtcars)  #cyl better than wt
summary(fit4)  # wt and cyl stronger, am bad

ncyl = 8
fit5 <- lm(mpg[cyl==ncyl] ~ am[cyl ==ncyl] + cyl[cyl == ncyl] +wt[cyl==ncyl] +disp[cyl==ncyl],data=mtcars)
summary(fit5)  # 

fit6 <- lm(mpg ~ am +cyl + wt +disp + wt*disp,data=mtcars)
summary(fit6)

anova(fit2,fit4, fit6)
#library(car)
#vif(fit3) #check for variance inflation

#havalues shows no outliers/influencersc
#Modeling multivariate relationships is difficult.
#Play around with simulations to see how the inclusion or exclustion of another variable can change
#analyses

plot(predict(fit4),resid(fit4))
