#Matrix Plots (figure 1):
pairs(~cnt+season+mnth_f+windspeed+hum+atemp+temp+weathersit+workingday+weekday+holiday)

#Diagnostic Plots of OLS model with all variables (figure 2 ~ 5):
m.ols.mnf <- lm(cnt~season+mnth_f+windspeed+hum+atemp+temp+weathersit+workingday+weekday+holiday)
plot(m.ols.mnf)

#All subsets Variable Selection:
m.final <- lm(cnt~season+mnth_f+holiday+weekday+workingday+weathersit+sr.at + sr.te + hum + hum.sq + sqrt(windspeed))

library(leaps)
library(car)
X<-model.matrix(m.final)
b<-regsubsets(as.matrix(X),cnt,data=data2011)
rs1 <- summary(b)
par(mfrow=c(1,1))
m1<-lm(cnt~X[,26])
m2<-lm(cnt~X[,26]+X[,25])
m3<- lm(cnt~X[,4]+X[,25]+X[,27])
m4<- lm(cnt~X[,4]+X[,24]+X[,25]+X[,27])
m5<- lm(cnt~X[,4]+X[,8]+X[,24]+X[,25]+X[,27])
m6<- lm(cnt~X[,2]+X[,3]+X[,4]+X[,24]+X[,25]+X[,27])
m7<- lm(cnt~X[,2]+X[,3]+X[,4]+X[,24]+X[,25]+X[,27]+X[,29])
m8<- lm(cnt~X[,2]+X[,3]+X[,4]+X[,8]+X[,9]+X[,24]+X[,25]+X[,27])
m9<- lm(cnt~X[,2]+X[,3]+X[,4]+X[,8]+X[,9]+X[,25]+X[,27]+X[,28]+X[,29])
n1 <- nrow(data2011)
lm.n <- function(l) {
  npar = length(coef(l)) + 1 
  AIC = extractAIC(l, k = 2)[2] 
  BIC = extractAIC(l, k = log(n1))[2] 
  return( c(rs1$adjr2[npar - 2], #adjr2 
            AIC, 
            AIC + 2 * npar * (npar + 1) / (n1 - npar + 1), # AICc 
            BIC))
}
matrix(unlist(lapply(list(m1, m2, m3, m4, m5, m6, m7,m8,m9),lm.n)), byrow = TRUE, ncol = 4, dimnames = list(1:9, c("adjr2", "AIC", "AICc", "BIC")))

#Summary table of m8 (table 2):
summary(m8)

#Diagnostic Plots of m8 (figure 6 ~ 9):
plot(m8)

#AR(1) model and ACF plot (figure 10):
re.mt <- rstandard(m8)
acf(re.mt)


library(nlme)
m.gls.f.ad <- gls(cnt~X[,2]+X[,3]+X[,4]+X[,8]+X[,9]+X[,24]+X[,25]+X[,27], 
                  data = data2011, correlation = corAR1(form =~instant), method = "ML")
summary(m.gls.f.ad)
rho.ar <- 0.3338995
X.ar <- model.matrix(m.ols.mnf)
Sigma.ar <- diag(length(instant))
Sigma.ar <- rho.ar^abs(row(Sigma.ar)-col(Sigma.ar))
sm.ar <- chol(Sigma.ar)
smi.ar <- solve(t(sm.ar))
xsta.ar <- smi.ar %*% X.ar
ystar.ar <- smi.ar %*% cnt
mltls.ar <- lm(ystar.ar~xsta.ar-1)
red.st.ar <- rstandard(mltls.ar)

#Inflation Factors Table (table 1):
vif(m8)

#Ridge Regression Table (table 2):
lambda <- 10^seq(10, -2, length = 100)
X.r <- X.ols[,-1]
library(glmnet)
train = data2011
test = data2012
ridge.mod <- glmnet(X.r, train$cnt, alpha = 0, lambda = lambda)
cv.out <- cv.glmnet(X.r, train$cnt, alpha = 0)
bestlamridge <- cv.out$lambda.min
predict(ridge.mod, type = "coefficients", s = bestlamridge)

#Lasso Regression Table (table 3):
X.l <- X[,-1]
lasso.mod <- glmnet(X.l, train$cnt, alpha = 1, lambda = lambda)
cv.out.L <- cv.glmnet(X.l, train$cnt, alpha = 1)
bestlamlasso <- cv.out.L$lambda.min
lasso.coef <- predict(lasso.mod, type = 'coefficients', s = bestlamlasso)

#Ridge Plot of Coefficients against Lambda (figure 11):
x <- data.frame(X[,2], X[,3], X[,4], X[,8], X[,9], X[,24], X[,25], X[,27])
x <- as.matrix(x)
y <- as.vector(cnt)
ridge.plot(y, x, lambda = 10^seq(3, -2, length = 100))

#Lasso Plot of Coefficients against Lambda (figure 12):
plot(lasso.mod, xvar="lambda", label = T)


