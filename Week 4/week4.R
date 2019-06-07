setwd("C:/Users/ROMEST/Downloads/homework/Week 4")

data <- read.table("uscrime.txt", header = T)

set.seed(888)


#### QUESTION 9.1 ####

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

# https://www.researchgate.net/post/Would_someone_advice_me_on_how_to_rescale_variables

# unscale the elements by multypling the values t(ik) with b(k)
unval <- pca.fit$rotation[, 1:8] %*% betas
a <- unval/sd_pred
a0 <- b0 - sum(unval*m_pred/sd_pred)
pred <- a0 + sum(a*p)




#### QUESTION 10.1 ####
### A ###

library(rpart)
library(rattle)
library(rpart.plot)


# model
tree.fit <- rpart(Crime ~ ., data = data)

# show variable importance
plot(tree.fit$variable.importance, xaxt = "n")
l <- names(tree.fit$variable.importance)
axis(1, at = 1:12, labels = l)

# plot tree
fancyRpartPlot(tree.fit)
rpart.plot(tree.fit)





### B ###

library(randomForest)
library(randomForestExplainer)

# model
rf.fit <- randomForest(Crime ~ ., data = data, ntree = 100, mtry = 4, localImp = TRUE)

# check variable importance
varImpPlot(rf.fit, main = "Variable Importance")

# tune hyperparameters
tun <- tuneRF(data[,-16], data$Crime, stepFactor = 0.5, plot = TRUE, ntreeTry = 1000, trace = TRUE, startmtry = 1, mtry = 4)

# explain RF
# explain_forest(rf.fit) # plot distribution of minimal depth and multi-way importance
min_depth_frame <- min_depth_distribution(rf.fit)
# k indicates the number of variables to plot
plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 19)
rf.importance <- measure_importance(rf.fit, mean_sample = "relevant_trees") # data frame with importance values (gini_decrease, times_a_root, etc.)
mw1 <- plot_multi_way_importance(rf.importance, size_measure = "no_of_nodes", no_of_labels = 5)

plot_importance_ggpairs(rf.importance)
plot_importance_rankings(rf.importance)
vars <- important_variables(rf.importance, k = 10, measures = c("mean_min_depth", "no_of_trees"))
rf.interactions <- min_depth_interactions(rf.fit, vars) 
plot_min_depth_interactions(rf.interactions) 




#### QUESTION 10.3 ####
data <- read.table("germancredit.txt", header = F)



