rm(list=ls())
library(faraway)
attach(sat)

help(sat)

data(sat,package = "faraway")
lmod <- lm(total~takers+ratio+salary+expend)

## Check the constant variance assumption for the errors and for evidence
## of non-linearity via residual plots, and adjust model as appropriate
par(mfrow=c(1,2))
plot(fitted(lmod),residuals(lmod),xlab="Fitted Values",ylab="Residuals")
abline(h=0) # a diagnostic residual plot
plot(fitted(lmod),abs(residuals(lmod)),xlab="Fitted Values",ylab="|Residuals|")
abline(h=0)

# discover which predictor has a non-linear relationship with the response 
par(mfrow=c(2,2))
plot(takers,residuals(lmod),xlab="Takers",ylab="Residuals")
abline(h=0)
plot(ratio,residuals(lmod),xlab="Ratio",ylab="Residuals")
abline(h=0)
plot(salary,residuals(lmod),xlab="Salary",ylab="Residuals")
abline(h=0)
plot(expend,residuals(lmod),xlab="Expend",ylab="Residuals")
abline(h=0)
par(mfrow=c(1,1))

lmod2 <- lm(total~takers+I(takers^2)+ratio+salary+expend)
plot(fitted(lmod2),residuals(lmod2),xlab="Fitted Values",ylab="Residuals")
abline(h=0)

par(mfrow=c(1,2))
plot(fitted(lmod),residuals(lmod),xlab="Fitted Values",ylab="Residuals",main ="original model")
abline(h=0)
plot(fitted(lmod2),residuals(lmod2),xlab="Fitted Values",ylab="Residuals",main = "new model")
abline(h=0)
par(mfrow=c(1,1))

# constant variance
summary(lm(abs(lmod$residual)~lmod$fitted.values))
summary(lm(abs(lmod2$residual)~lmod2$fitted.values))

## Check the normality assumption.
qqnorm(lmod2$residuals,ylab = "Residuals")
qqline(lmod2$residuals)
shapiro.test(lmod2$residuals)
## Check for large leverage points.
## half-normal plot for leverages
halfnorm(lm.influence(lmod2)$hat,nlab=2,ylab = "Leverage")
sat[c(7,44),]
## Check for outliers.
ti<-rstudent(lmod2)
max(abs(ti))
which(ti==max(abs(ti)))
2*(1-pt(max(abs(ti)),df=50-4-1))
0.05/50




