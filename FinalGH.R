# BSAN 450 Final
# Grant Healy

library(TSA)
library(xts)
library(lmtest)
library(ISLR)
library(dplyr)
library(randomForest)
library(gbm)
library(tree)


# Question 1 ----

# Read in Data

souvenir <- read.csv("souvenir.csv")
attach(souvenir)


# Initial Plot

par(mfrow=c(1,1))
plot(Sales, ylab='Sales', xlab='Month', type='o')


# Variability Difference Fix

BoxCox.ar(y=Sales, method = "yule-walker")

souvenir$logSales <- log(Sales)

plot(souvenir$logSales, ylab='Log Sales', xlab='Month', type='o')



# Determining Differences

par(mfrow=c(1,1))
plot(diff(souvenir$logSales), ylab='Log Sales 1st Dif', xlab='Month', type='o')

par(mfrow=c(1,2))
acf(diff(souvenir$logSales), lag.max=36)
pacf(diff(souvenir$logSales), lag.max=36)


par(mfrow=c(1,1))
plot(diff(souvenir$logSales,lag=12), ylab='Log Sales 12th Dif', xlab='Month', type='o')

par(mfrow=c(1,2))
acf(diff(souvenir$logSales,lag=12), lag.max=36)
pacf(diff(souvenir$logSales,lag=12), lag.max=36)


par(mfrow=c(1,1))
plot(diff(diff(souvenir$logSales),lag=12), ylab='Log Sales 1st and 12th', xlab='Month', type='o')

par(mfrow=c(1,2))
acf(diff(diff(souvenir$logSales,lag=12)), lag.max=36)
pacf(diff(diff(souvenir$logSales,lag=12)), lag.max=36)


# Model Fitting

souvenir.fit1 = arima(souvenir$logSales, order=c(1,1,0), seasonal=list(order=c(0,1,0), period=12))
souvenir.fit1
coeftest(souvenir.fit1)
Box.test(residuals(souvenir.fit1),lag=10, type="Ljung", fitdf=1)


souvenir.fit2 = arima(souvenir$logSales, order=c(0,1,1), seasonal=list(order=c(0,1,0), period=12))
souvenir.fit2
coeftest(souvenir.fit2)
Box.test(residuals(souvenir.fit2),lag=10, type="Ljung", fitdf=1)


souvenir.fit3 = arima(souvenir$logSales, order=c(0,1,1), seasonal=list(order=c(0,1,2), period=12))
souvenir.fit3
coeftest(souvenir.fit3)
Box.test(residuals(souvenir.fit3),lag=10, type="Ljung", fitdf=3)

# Residual Diagnostics

par(mfrow = c(2,2))
plot(rstandard(souvenir.fit1), ylab='Standardized Residuals', type='o')
hist(rstandard(souvenir.fit1))
qqnorm(residuals(souvenir.fit1))
qqline(residuals(souvenir.fit1))
pacf(rstandard(souvenir.fit1))


par(mfrow = c(2,2))
plot(rstandard(souvenir.fit2), ylab='Standardized Residuals', type='o')
hist(rstandard(souvenir.fit2))
qqnorm(residuals(souvenir.fit2))
qqline(residuals(souvenir.fit2))
acf(rstandard(souvenir.fit2))


par(mfrow = c(2,2))
plot(rstandard(souvenir.fit3), ylab='Standardized Residuals', type='o')
hist(rstandard(souvenir.fit3))
qqnorm(residuals(souvenir.fit3))
qqline(residuals(souvenir.fit3))
acf(rstandard(souvenir.fit3))


# Outliers

detectAO(souvenir.fit2)
detectIO(souvenir.fit2)


# Prediction Check

predict(souvenir.fit2, n.ahead=24)

par(mfrow=c(1,1))
plot(souvenir.fit2, n.ahead=24, type='b', col='red')


# Question 2 ----

# Read in Data

emales <- read.csv("emales.CSV")
attach(emales)


# Initial Plot

par(mfrow=c(1,1))
plot(Employed, ylab='Employed Males', xlab='Month', type='o')


# Determining Differences

par(mfrow=c(1,1))
plot(diff(Employed), ylab='Log Employed 1st Dif', xlab='Month', type='o')

par(mfrow=c(1,2))
acf(diff(Employed), lag.max=36)
pacf(diff(Employed), lag.max=36)


par(mfrow=c(1,1))
plot(diff(Employed,lag=12), ylab='Log Employed 12th Dif', xlab='Month', type='o')

par(mfrow=c(1,2))
acf(diff(Employed,lag=12), lag.max=36)
pacf(diff(Employed,lag=12), lag.max=36)


par(mfrow=c(1,1))
plot(diff(diff(Employed),lag=12), ylab='Log Sales', xlab='Month', type='o')

par(mfrow=c(1,2))
acf(diff(diff(Employed,lag=12)), lag.max=36)
pacf(diff(diff(Employed,lag=12)), lag.max=36)


# Model Fitting

employed.fit1 = arima(Employed, order=c(0,1,1), seasonal=list(order=c(0,1,1), period=12))
employed.fit1
coeftest(employed.fit1)
Box.test(residuals(employed.fit1),lag=10, type="Ljung", fitdf=2)


employed.fit2 = arima(Employed, order=c(1,1,0), seasonal=list(order=c(1,1,0), period=12))
employed.fit2
coeftest(employed.fit2)
Box.test(residuals(employed.fit2),lag=10, type="Ljung", fitdf=2)


employed.fit3 = arima(Employed, order=c(1,1,0), seasonal=list(order=c(2,1,0), period=12))
employed.fit3
coeftest(employed.fit3)
Box.test(residuals(employed.fit3),lag=10, type="Ljung", fitdf=3)


# Residual Diagnostics

par(mfrow = c(2,2))
plot(rstandard(employed.fit1), ylab='Standardized Residuals', type='o')
hist(rstandard(employed.fit1))
qqnorm(residuals(employed.fit1))
qqline(residuals(employed.fit1))
acf(rstandard(employed.fit1))


par(mfrow = c(2,2))
plot(rstandard(employed.fit2), ylab='Standardized Residuals', type='o')
hist(rstandard(employed.fit2))
qqnorm(residuals(employed.fit2))
qqline(residuals(employed.fit2))
pacf(rstandard(employed.fit2))


par(mfrow = c(2,2))
plot(rstandard(employed.fit3), ylab='Standardized Residuals', type='o')
hist(rstandard(employed.fit3))
qqnorm(residuals(employed.fit3))
qqline(residuals(employed.fit3))
pacf(rstandard(employed.fit3))


# Outliers

detectAO(employed.fit1)
detectIO(employed.fit1)

detectAO(employed.fit3)
detectIO(employed.fit3)

# Resolve Outliers

employed.fitIO1 = arima(Employed, order=c(0,1,1), io=c(18,21), seasonal=list(order=c(0,1,1), period=12))
employed.fitIO1
coeftest(employed.fitIO1)
Box.test(residuals(employed.fitIO1),lag=10, type="Ljung", fitdf=4)

par(mfrow = c(2,2))
plot(rstandard(employed.fitIO1), ylab='Standardized Residuals', type='o')
hist(rstandard(employed.fitIO1))
qqnorm(residuals(employed.fitIO1))
qqline(residuals(employed.fitIO1))
acf(rstandard(employed.fitIO1))

detectAO(employed.fitIO1)
detectIO(employed.fitIO1)


employed.fitIO3 = arima(Employed, order=c(1,1,0), io=c(18,21), seasonal=list(order=c(2,1,0), period=12))
employed.fitIO3
coeftest(employed.fitIO3)
Box.test(residuals(employed.fitIO3),lag=10, type="Ljung", fitdf=5)

par(mfrow = c(2,2))
plot(rstandard(employed.fitIO3), ylab='Standardized Residuals', type='o')
hist(rstandard(employed.fitIO3))
qqnorm(residuals(employed.fitIO3))
qqline(residuals(employed.fitIO3))
pacf(rstandard(employed.fitIO3))

detectAO(employed.fitIO3)
detectIO(employed.fitIO3)

# Question 3 ----

# Read in Data

ames <- read.csv("ames-1.csv")
attach(ames)


# Cleaning Data

ames <- na.omit(ames)
ames <- mutate_if(ames, is.character, as.factor)


# Hist

par(mfrow=c(1,1))
hist(ames$Sale_Price)
hist(log(ames$Sale_Price))


# A. Split the Data

set.seed(1)
idx = sample(1:nrow(ames), ceiling(nrow(ames)/2))
ames.train = ames[idx,]
ames.test = ames[-idx,]


# B. Initial Tree with All Variables 

tree.ames = tree(log(Sale_Price) ~ ., data = ames.train)
summary(tree.ames)

plot(tree.ames)
text(tree.ames, pretty = 0)
title("All Features Tree")


# C. Tree Pruning 

set.seed(1)
cv.tree.ames=cv.tree(tree.ames)
cv.tree.ames

plot(cv.tree.ames$size, cv.tree.ames$dev, type="b")

cv.tree.ames$size[which.min(cv.tree.ames$dev)]

prune.cv.mytree = prune.tree(tree.ames, best = 7)
summary(prune.cv.mytree)

plot(prune.cv.mytree)
text(prune.cv.mytree, pretty = 0)
title("Pruned Tree")


# C2. MSE Comparison

yhat.pruned <- predict(prune.cv.mytree, ames.test)

mse.pruned <- mean((exp(yhat.pruned) - (ames.test$Sale_Price))^2)
mse.pruned

yhat.unpruned <- predict(tree.ames, ames.test)

mse.unpruned <- mean((exp(yhat.unpruned) - (ames.test$Sale_Price))^2)
mse.unpruned


# D. Bagged Tree

set.seed(1)
bag.ames = randomForest(log(Sale_Price) ~ ., data = ames.train, mtry=80, importance =TRUE)
bag.ames

yhat.bag = predict(bag.ames , newdata = ames.test)

mse.bag <- mean((exp(yhat.bag) - (ames.test$Sale_Price))^2)
mse.bag

# E. Random Forrest

set.seed(1)
rf.ames = randomForest(log(Sale_Price) ~ ., data = ames.train, mtry=27, importance =TRUE)
rf.ames

yhat.rf = predict(rf.ames, newdata = ames.test)
mse.rf <- mean((exp(yhat.rf) - (ames.test$Sale_Price))^2)
mse.rf

# F. Boosted Tree

set.seed(1)
boost.ames1 = gbm(log(Sale_Price) ~ ., data=ames.train, distribution="gaussian", 
                  n.trees=500, interaction.depth=4)
boost.ames1

yhat.boost1 = predict(boost.ames1 , newdata = ames.test, n.trees=500)

mse.boost1 <- mean((exp(yhat.boost1)-(ames.test$Sale_Price))^2)
mse.boost1


set.seed(1)
boost.ames2 = gbm(log(Sale_Price) ~ ., data=ames.train, distribution="gaussian", 
                  n.trees=1000, interaction.depth=6)
boost.ames2

yhat.boost2 = predict(boost.ames2 , newdata = ames.test, n.trees=1000)

mse.boost2 <- mean((exp(yhat.boost2)-(ames.test$Sale_Price))^2)
mse.boost2


# G. Tree Comparison

# Unpruned
mse.unpruned
# 1,544,940,571

# Pruned
mse.pruned
# 1,846,488,155

# Bagged
mse.bag
# 613,284,495

# Random Forrest 
mse.rf
# 589,629,951

# Boosted
mse.boost1
# 415,987,831

mse.boost2
# 442,262,217


# Question 4 ----

# Read in Data

nci.labs=NCI60$labs
nci.data=NCI60$data


# A. Precomp with PCA Function / Scale = True

pr.out = prcomp(nci.data, scale=TRUE)


# B. First Two Principal Component Plots

par(mfrow =c(1,2))

plot(pr.out$x [,1:2], col=as.factor(nci.labs), pch =19,
     xlab ="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3) ], col=as.factor(nci.labs), pch =19,
     xlab ="Z1",ylab="Z3")


# C. PVE and Cumulative PVE Plots

pve =100* pr.out$sdev ^2/ sum(pr.out$sdev ^2)


par(mfrow =c(1,2))

plot(pve , type ="o", ylab="PVE ", xlab=" Principal Component ",
     col =" blue")
plot(cumsum (pve ), type="o", ylab =" Cumulative PVE", xlab="
Principal Component ", col =" brown3 ")


# D. Scaling and K-Means Clustering

scaled_nci_data <- scale(nci.data)

mean(scaled_nci_data)
sd(scaled_nci_data)

set.seed(1)
NCI_data_K4 <- kmeans(scaled_nci_data, 4, nstart = 20)

par(mfrow = c(1,1))
plot(scaled_nci_data, col =(NCI_data_K4$cluster +1 ) , main="K-Means Clustering Results with K=4",
     xlab ="", ylab="", pch =20, cex =1)


NCI_data_K4$size

100*NCI_data_K4$betweenss/NCI_data_K4$totss








