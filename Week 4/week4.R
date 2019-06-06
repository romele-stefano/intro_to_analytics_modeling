setwd("C:/Users/ROMEST/Downloads/homework/Week 4")

data <- read.table("uscrime.txt", header = T)

set.seed(888)
#### PCA ####
# scale = (x-mean(x))/std(x)
pca.fit <- prcomp(data[,-16], scale = TRUE)

# plot PCA
biplot(pca.fit)

# plot variance reduction for each components. We can see the optimal number of components is 8.
# if we look at the summary(pca.fit), we can see that starting PCA8 the proportion of variance explained is
# less than 1.6%. The cumulative variance explained with 8 components is equal to 94%.
plot(pca.fit, xlab = "Principal components", main = "Variance reduction by principal component")

# create data set with response variable and principal components
data.pca <- cbind(pca.fit$x[,1:8], data$Crime)
data.pca <- as.data.frame(data.pca)

# new data point
p <- cbind(14,0,10,12,15.5,0.640,94,150,1.1,0.120,3.6,3200,20.1,0.04,39)
p <- as.data.frame(p)
colnames(p) <- c("M","So","Ed","Po1","Po2","LF","M.F","Pop","NW","U1","U2","Wealth","Ineq","Prob","Time")


# fit a linear regression model
lm.fit <- lm(V9 ~ ., data = data.pca)

# the linear regression models find the coefficients b0, ..., bL
# intercept
b0 <- lm.fit$coefficients[1]
# others coefficients (PC scores)
betas <- lm.fit$coefficients[2:9]

# https://stats.stackexchange.com/questions/229092/how-to-reverse-pca-and-reconstruct-original-variables-from-several-principal-com
# PCA reconstruction = PC scores * Eigenvectors(transposed) + Mean

# find mean and standard deviation of predictors
m_pred <- sapply(data[,1:15], mean)
sd_pred <- sapply(data[,1:15], sd)


# Perhaps the most simple, quick and direct way to mean-center your data is by using the 
# function scale(). By default, this function will standardize the data (mean zero, unit variance). 
# To indicate that we just want to subtract the mean, we need to turn off the argument scale = FALSE.
# unscale the elements by multypling the values t(ik) with b(k)
unval <- pca.fit$rotation[, 1:8] %*% betas
a <- unval/sd_pred
a0 <- b0 - sum(unval*m_pred/sd_pred)
pred <- a0 + sum(a*p)


