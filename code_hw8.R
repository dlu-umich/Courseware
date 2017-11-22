rm(list=ls())
library(faraway)
attach(seatpos)
help("seatpos")
head(seatpos)
rmse <- function(x,y){ sqrt(mean((x-y)^2)) }
smp_size <- floor(0.8 * nrow(seatpos))
set.seed(123)
train_ind <- sample(seq_len(nrow(seatpos)), size = smp_size)
train <- seatpos[train_ind, ]
test <- seatpos[-train_ind, ]

## 1. Linear regression with all predictors
model <- lm(hipcenter ~ ., train)
sumary(model)
rmse(model$fit,train$hipcenter)
rmse(predict(model,newdata = test),test$hipcenter)
library(DAAG)
help(cv.lm)
cv.lm(data = train, model, m=3, dots = FALSE, seed=123, plotit=TRUE, printit=TRUE)

## 2. Linear regression with variables selected using AIC
model_aic <- step(model)
sumary(model_aic)
rmse(model_aic$fit,train$hipcenter)
rmse(predict(model_aic,newdata = test),test$hipcenter)
cv.lm(data = seatpos, model_aic, m=3, dots = FALSE, seed=123, plotit=TRUE, printit=TRUE)

## 3. Principal component regression - using CV to pick order of model
library(stats)
seatpca <- prcomp(train[,-9])
round(seatpca$sdev,3)
# plot loading of each x to be z, because z = u * x
matplot(1:8,seatpca$rotation[,1:3],type = "l", xlab = "Frequency", ylab = "", lwd =3)
matplot(1:8,seatpca$sdev[1:8],type = "l", xlab = "PC numbers", ylab = "SD of PC")
# choose min mse of k pcs
library(pls)
model_pcr <- pcr(hipcenter~.,data = train, ncomp = 8)
rms_seat = NULL
for (k in 1:8){
  pv = predict(model_pcr, newdata = test, ncomp = k)
  rms_seat[k] = rmse(pv, test$hipcenter)
}
plot(rms_seat,xlab = "PC number", ylab = "Test RMS", type="l")
# min point to select k
which.min(rms_seat)
rms_seat[5]
# do cross-validation
model_pcr2 <- pcr(hipcenter~.,data = train, ncomp = 8, validation = "CV", segments = 10)
rms_CV = RMSEP(model_pcr2,estimate = "CV")
which.min(rms_CV$val)
plot(rms_CV$val, xlab = "PC number", ylab = "CV RMS", type="l")
rmse(predict(model_pcr2, newdata = test, ncomp = 4), test$hipcenter)

## 4. Partial least squares - using CV to pick order of model
model_pls = plsr(hipcenter ~ ., data = train, ncomp = 8, validation = "CV")
pls_rmsCV = RMSEP(model_pls, estimate = "CV")
plot(pls_rmsCV$val, xlab = "PC number", ylab = "CV RMS", type="l")
which.min(pls_rmsCV$val)
dim(model_pls$fitted.values)
rmse(model_pls$fitted.values[,,4], train$hipcenter)
# on test data
dim(predict(model_pls, newdata = test))
rmse(predict(model_pls, newdata = test)[,,4], test$hipcenter)

## 5. Ridge regression - using GCV to pick regularization parameter lambda
library(MASS)
# select an appropriate lambda
model_ridge = lm.ridge(hipcenter ~ .,lambda = seq(0, 50, 1e-2), data = train)
matplot(model_ridge$lambda, t(model_ridge$coef), type = "l", lty = 1, 
        xlab = expression(lambda), ylab = expression(hat(beta)))
select(model_ridge) 
abline(v = 22.7)
# compute fitted values
yfit = model_ridge$ym + scale(train[,-9], center = model_ridge$xm, 
                              scale = model_ridge$scales) %*% model_ridge$coef[,4]# why is 4?
rmse(yfit,train$hipcenter)
ypred = model_ridge$ym + scale(test[,-9], center = model_ridge$xm, 
                               scale = model_ridge$scales) %*% model_ridge$coef[,4]
rmse(ypred, test$hipcenter)

## 6. Lasso regression - using CV to pick regularization parameter t
require(lars)
model_lasso <- lars(as.matrix(train[,-9]),train$hipcenter)
plot(model_lasso)
# do cross validation
set.seed(123)
cv_lasso <- cv.lars(as.matrix(train[,-9]), train$hipcenter)
cv_lasso$index[which.min(cv_lasso$cv)]
predict(model_lasso, s = 0.222, type = "coef", mode = "fraction")$coef
coef(lm(hipcenter ~ Age + Ht + Leg, train))
# on test data
testx <- as.matrix(test[,-9])
predlars <- predict(model_lasso, testx, s = 0.222, mode ="fraction")
rmse(test$hipcenter, predlars$fit)
predlars <- predict(model_lasso, s = 0.222, type = "coef", mode ="fraction")
plot(predlars$coef, type = "h", ylab = "Coefficient")












# diagnostic for model in (a) model_t
par(mfrow=c(2,2))
# Check normality with Q-Q plot
qqnorm(model$residual, ylab="Residuals")
qqline(model$residual)
# Residuals against fitted values
plot(model$fitted.values,model$residuals,hline = 0, xlab="fitted values", ylab="Residuals", main = "Residuals")
outlier <- which.max(abs(lmodel$residuals))
text(model$fitted.values[outlier],model$residuals[outlier],labels = row.names(trees)[outlier])
# A half-normal plot will help identify leverage points.
halfnorm(lm.influence(model)$hat,labs=row.names(seatpos),nlab =2, ylab="Leverages", title("Leverages"))
# use jackknife residuals and the Bonferroni correction to see any outliers
# Cook distance
cook = cooks.distance(model)
halfnorm(cook,labs=row.names(seatpos),ylab="Cook¡¯s distance",nlab =2,title("Cook's distance"))
# conclusion > no outliners


