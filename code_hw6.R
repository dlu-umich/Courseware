rm(list=ls())
library(faraway)
attach(sat)
# Ordinary least squares
lmod_ols <- lm(total~takers+ratio+salary+expend)
sumary(lmod_ols)
# Least absolute deviations
library(quantreg)
lmod_lad <- rq(total~takers+ratio+salary+expend)
summary(lmod_lad)
# Huber's robust regression
library(MASS)
lmod_huber <- rlm(total~takers+ratio+salary+expend)
summary(lmod_huber)
# Least trimmed squares
library(MASS)
lmod_lts <- ltsreg(total~takers+ratio+salary+expend, nsamp = 'exact')
round(lmod_lts$coef,4)
# Bootstrap to get confidence interval of LTS method
x <- sat[,c(4,2,3,1)]
bcoef <- matrix(0, nrow = 1000, ncol = 5)
for (i in 1:1000){
  newy <- lmod_lts$fit+lmod_lts$residuals[sample(50,rep=T)]
  bcoef[i,] <- ltsreg(x,newy,nsamp = "best")$coef
}
# 95% CI for parameters
colnames(bcoef) <- names(coef(lmod_lts))
apply(bcoef,2,function(x) quantile(x,c(0.025,0.975)))
library(ggplot2)
bcoef <- data.frame(bcoef)
library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)
plot1<-ggplot(bcoef,aes(x=takers))+geom_density() + geom_vline(xintercept = c(-3.75,-2.41),linetype="dashed")
plot2<-ggplot(bcoef,aes(x=ratio))+geom_density() + geom_vline(xintercept = c(-19.68,0.10),linetype="dashed")
plot3<-ggplot(bcoef,aes(x=salary))+geom_density() + geom_vline(xintercept = c(-5.24,9.05),linetype="dashed")
plot4<-ggplot(bcoef,aes(x=expend))+geom_density() + geom_vline(xintercept = c(-18.97,42.46),linetype="dashed")
# diagnostic and remove outliers and influential points
par(mfrow=c(2,2))
# Check normality
qqnorm(lmod_ols$residual, ylab="Residuals")
qqline(lmod_ols$residual)
plot(lmod_ols$fitted.values,lmod_ols$residuals,xlab="fitted values", ylab="Residuals", main = "Residuals")
outlier <- which.max(abs(lmod_ols$residuals))
text(lmod_ols$fitted.values[outlier],lmod_ols$residuals[outlier],labels = row.names(sat)[outlier])
# A half-normal plot will help identify leverage points.
halfnorm(lm.influence(lmod_ols)$hat,labs=row.names(sat),ylab="Leverages")
# use jackknife residuals and the Bonferroni correction to see any outliers
jack<-rstudent(lmod_ols)
jack[order(abs(jack),decreasing=TRUE)][1:5]
qt(0.05/(50*2),44)
# Cook distance
cook = cooks.distance(lmod_ols)
halfnorm(cook,labs=row.names(sat),ylab="Cook¡¯s distance")
# remove obs Utah and redo regression
lmod_ols_2 <- lm(total~takers+ratio+salary+expend,subset = (row.names(sat)!="Utah"))
sumary(lmod_ols_2)

