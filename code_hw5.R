rm(list=ls())
library(faraway)
attach(cheddar)

help(cheddar)

data("cheddar",package = "faraway")
lmod <- lm(taste~Acetic+H2S+Lactic)
summary(lmod)


## Check the constant variance assumption for the errors and for evidence
## of non-linearity via residual plots, and adjust model as appropriate
par(mfrow=c(1,2))
plot(fitted(lmod),residuals(lmod),xlab="Fitted Values",ylab="Residuals")
abline(h=0) # a diagnostic residual plot
plot(fitted(lmod),abs(residuals(lmod)),xlab="Fitted Values",ylab="|Residuals|")
abline(h=0)
summary(lm(abs(lmod$residual)~lmod$fitted))

# discover which predictor has a non-linear relationship with the response 
par(mfrow=c(1,3))
plot(Acetic,residuals(lmod),xlab="Acetic",ylab="Residuals")
abline(h=0)
plot(H2S,residuals(lmod),xlab="H2S",ylab="Residuals")
abline(h=0)
plot(Lactic,residuals(lmod),xlab="Lactic",ylab="Residuals")
abline(h=0)
par(mfrow=c(1,1))

# partial regression plots
delta <- residuals(lm(taste~H2S+Lactic))
gamma <- residuals(lm(Acetic~H2S+Lactic))
plot(gamma,delta,xlab="Acetic Residuals",ylab="taste residuals")
temp <- lm(delta ~ gamma)
coef(temp)
coef(lmod)
abline(reg=temp)

delta2 <- residuals(lm(taste~Acetic+Lactic))
gamma2 <- residuals(lm(H2S~Acetic+Lactic))
plot(gamma,delta,xlab="H2S Residuals",ylab="taste residuals")
temp <- lm(delta2 ~ gamma2)
sumary(temp)
coef(lmod)
abline(reg=temp)

delta3 <- residuals(lm(taste~Acetic+H2S))
gamma3 <- residuals(lm(Lactic~Acetic+H2S))
plot(gamma,delta,xlab="Lactic Residuals",ylab="taste residuals")
temp <- lm(delta3 ~ gamma3)
sumary(temp)
coef(lmod)
abline(reg=temp)



# partial residual plot
plot(Acetic,lmod$residuals+coef(lmod)['Acetic']*Acetic,xlab="Acetic",ylab="taste(adjusted for Acetic)")
abline(a=0,b=coef(lmod)["Acetic"])

lmod2 <- lm(taste~Acetic+H2S+Lactic)
summary(lmod2)
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
qqnorm(lmod$residuals,ylab = "Residuals")
qqline(lmod$residuals)
shapiro.test(lmod$residuals)
## Check for large leverage points.
## half-normal plot for leverages
par(mfrow=c(1,2))
halfnorm(lm.influence(lmod)$hat,ylab = "Leverage")
abline(h=2*(3+1)/30)
qqnorm(rstandard(lmod))
abline(0,1)
## Check for outliers.
ti<-rstudent(lmod)
cheddar[which.max(abs(ti)),]
2*(1-pt(max(abs(ti)),df=30-(3+1)-1))
0.05/30

# check cook distance
cook<-cooks.distance(lmod)
halfnorm(cook,nlab = 6, ylab="Cook's Distance")
lmod.15<-lm(taste~.,data = cheddar,subset=(cook<max(cook)))
summary(lmod.15)
summary(lmod)

inf = lm.influence(lmod)
plot(inf$coef[,2],inf$coef[,3])



### 2. (b) studentized residual instead of standard residuals
par(mfrow=c(1,3))
plot(fitted(lmod2),residuals(lmod2),xlab="Fitted Values",ylab="Residuals")
abline(h=0)
plot(fitted(lmod2),rstudent(lmod2),xlab="Fitted Values",ylab="Studentized Residuals")
abline(h=0)
plot(fitted(lmod2),rstandard(lmod2),xlab="Fitted Values",ylab="Standard Residuals")
abline(h=0) # a diagnostic residual plot
