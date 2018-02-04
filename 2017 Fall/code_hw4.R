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
summary(lm(abs(lmod$residual)~lmod$fitted))

par(mfrow=c(1,2))
plot(fitted(lmod),residuals(lmod),xlab="Fitted Values",ylab="Residuals")
abline(h=0) # a diagnostic residual plot
plot(fitted(lmod),sqrt(abs(residuals(lmod))), xlab="Fitted Values",ylab= expression(sqrt(hat(epsilon))))
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
plot(fitted(lmod2),residuals(lmod2),xlab="Fitted Values",ylab="Residuals")
abline(h=0) # a diagnostic residual plot
plot(fitted(lmod2),sqrt(abs(residuals(lmod2))), xlab="Fitted Values",ylab= expression(sqrt(hat(epsilon))))
abline(h=0)
par(mfrow=c(1,1))

# constant variance
summary(lm(sqrt(abs(lmod$residual))~lmod$fitted.values))
summary(lm(sqrt(abs(lmod2$residual))~lmod2$fitted.values))

## check correlated errors
n <- length(residuals(lmod2))
plot(tail(residuals(lmod2),n-1) ~ head(residuals(lmod2),n-1), xlab=
       expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
abline(h=0,v=0,col=grey(0.75))
sumary(lm(tail(residuals(lmod2),n-1) ~ head(residuals(lmod2),n-1) -1))

## Check the normality assumption.
qqnorm(lmod2$residuals,ylab = "Residuals")
qqline(lmod2$residuals)
shapiro.test(lmod2$residuals)
## Check for large leverage points.
## half-normal plot for leverages
par(mfrow=c(1,2))
halfnorm(lm.influence(lmod2)$hat,nlab=2,ylab = "Leverage")
abline(h=2*(5+1)/50)
qqnorm(rstandard(lmod))
abline(0,1)
## Check for outliers.
ti<-rstudent(lmod2)
sat[which.max(abs(ti)),]
2*(1-pt(max(abs(ti)),df=50-(5+1)-1))
0.05/50

summary(lm(rstandard(lmod2)~lmod2$fitted.values))
### 2. (b) studentized residual instead of standard residuals
par(mfrow=c(1,3))
plot(fitted(lmod2),residuals(lmod2),xlab="Fitted Values",ylab="Residuals")
abline(h=0)
plot(fitted(lmod2),rstudent(lmod2),xlab="Fitted Values",ylab="Studentized Residuals")
abline(h=0)
plot(fitted(lmod2),rstandard(lmod2),xlab="Fitted Values",ylab="Standard Residuals")
abline(h=0) # a diagnostic residual plot
