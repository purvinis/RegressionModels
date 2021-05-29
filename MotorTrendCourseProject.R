require(datasets)
data(mtcars)
library(ggplot2)
require(ggplot2)
install.packages("GGally")
require(GGally)
install.packages("rgl")
library(rgl)
g1head(mtcars)
#“Is an automatic or manual transmission better for MPG”
# confounders: 

fitall <- lm(mpg ~ ., data=mtcars)
summary(fitall)

g1 <- ggpairs(mtcars,lower = list(continuous = wrap("points")),ggplot2::aes(colour=factor(am)))
g1 <- g1 + ggtitle("Data and correlation matrix, colored by regressor 'am'")
g2 <- ggcorr(mtcars, method = c("everything","spearman"))
g2
# Scatterplots of each pair of numeric variable are drawn on the left part 
#of the figure. Pearson correlation is displayed on the right. 
#Variable distribution is available on the diagonal.

fit1 <- lm(mpg ~ am , data=mtcars)
signif(summary(fit1)$coef[,c(1,4)],digits =5) 

fit2 <- lm(mpg ~ am + cyl ,data=mtcars)
signif(summary(fit2)$coef[,c(1,4)],digits =5)

fit3 <- lm(mpg ~ am + cyl + wt, data=mtcars)
signif(summary(fit3)$coef[,c(1,4)],digits =5) 

fit2 <- lm(mpg ~ am + cyl ,data=mtcars)
signif(summary(fit2)$coef[,c(1,4)],digits =5)

#e2 <- resid(fit2)

fit3 <- lm(mpg[am==1] ~ cyl[am ==1] + wt[am ==1],data=mtcars)
summary(fit3)  # bad am p-value 
#value of the relationship between mpg and cyl remains
#largely unchanged when considering am
#check the residual variation. Should be almost no resid variability
#after accounting for am

fit4 <- lm(mpg ~ am + cyl ,data=mtcars)  #cyl better than wt
summary(fit4)  # wt and cyl stronger, am bad

fit5 <- lm(mpg ~ am +  hp ,data=mtcars)
signif(summary(fit5)$coef[,c(1,4)],digits =5)

par(mfrow=c(1,3))
plot(predict(fit5),resid(fit5))
plot(mtcars$am,resid(fit5))
plot(mtcars$hp,resid(fit5))
mean(resid(fit5)[mtcars$am==0])
mean(resid(fit5)[mtcars$am==1])
mean(resid(fit5))
var(resid(fit5)[mtcars$am==0])
var(resid(fit5)[mtcars$am==1])

plot3d(mtcars$am,mtcars$hp,mtcars$mpg)


anova(fit2,fit4, fit6)
#library(car)
#vif(fit3) #check for variance inflation

#havalues shows no outliers/influencersc
#Modeling multivariate relationships is difficult.
#Play around with simulations to see how the inclusion or exclustion of another variable can change
#analyses

plot(predict(fit4),resid(fit4))
range(mtcars$hp)  #52, 335 #create some hp data
newhp0 <- data.frame(hp =c(52:335), am = 0)
newhp1 <- data.frame(hp =c(52:335), am = 1)
ci0 <- predict(fit5,newdata= newhp0, interval = "confidence")
ci1 <- predict(fit5,newdata= newhp1, interval = "confidence")

par(mfrow=c(1,2))
plot(c(50,340),c(0,30), type = "n", xlab="hp|am=0", ylab= "mpg", main= "CI, am =0,auto") #set up
points(c(52:335),ci0[,1],col = "black", bg = "black",pch = 21)
points(c(52:335),ci0[,2],col = "lightblue",pch = 21)
points(c(52:335),ci0[,3],col = "lightblue",pch = 21)
points(mtcars$hp[mtcars$am==0],mtcars$mpg[mtcars$am==0],col = "darkgreen", bg = "darkgreen",pch = 22)
plot(c(50,340),c(0,30), type = "n", xlab="hp|am=1", ylab= "mpg", main= "CI, am =1,manual") #set up
points(c(52:335),ci1[,1],col = "red", bg = "red",pch = 21)
points(c(52:335),ci1[,2],col = "orange",pch = 21)
points(c(52:335),ci1[,3],col = "orange",pch = 21)
points(mtcars$hp[mtcars$am==1],mtcars$mpg[mtcars$am==1],col = "darkgreen", bg = "darkgreen",pch = 22)
