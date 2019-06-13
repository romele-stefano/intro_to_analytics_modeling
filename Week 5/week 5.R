setwd("C:/Users/ROMEST/Downloads/homework/Week 5")

# for lasso and elastich net
library(glmnet)

data <- read.table("uscrime.txt", header = T)


# STEPWISE REGRESSION #

# create model with all predictors
lm.fit.full <- lm(Crime ~ .,data = data)
# create model with only the intercept
lm.fit.initial <- lm(Crime ~ 1, data = data)

# create stepwise
step.fit <- step(lm.fit.initial, scope = list(lower = lm.fit.initial, upper = lm.fit.full), direction = "both")
summary(step.fit)
step.fit$anova


# LASSO #

# create matrix form
data.scaled = as.matrix(data)

# scale the data
 for (i in 1:15) {
   #scale predictors
   data.scaled[, i] = (data[, i] - min(data[, i])) / (max(data[, i]) - min(data[, i]))
}

# split in predictors and response
predictors <- data.scaled[,1:15]
response <- data.scaled[,16]

# model: lasso
lasso <- glmnet(predictors, response, family = "mgaussian", alpha = 1)
plot(lasso)

# find best lambda value
lasso.fit <- cv.glmnet(predictors, response)
plot(lasso.fit)
# find which variables to use 
coef(lasso.fit, s = lasso.fit$lambda.min)
coef(lasso.fit, s = lasso.fit$lambda.1se)


# Df is the number of nonzero coefficients
# %Dev is the % of deviance explained
# Lambda is the lambda valued tried
print(lasso)
print(lasso.fit)



# model: ridge regression
ridge <- glmnet(predictors, response, family = "mgaussian", alpha = 0)

