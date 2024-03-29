setwd("C:/Users/ROMEST/Downloads/homework/Week 5")
setwd("C:/USers/romel/Downloads/Week 5")

set.seed(888)

#### QUESTION 11.1


# for lasso and elastich net
library(glmnet)
# for plotting variables
library(plotmo)

data <- read.table("uscrime.txt", header = T)


# STEPWISE REGRESSION #

# create model with all predictors
lm.fit.full <- lm(Crime ~ .,data = data)
# create model with only the intercept
lm.fit.initial <- lm(Crime ~ 1, data = data)

# create stepwise
step.fit <- step(lm.fit.initial, scope = list(lower = lm.fit.initial, upper = lm.fit.full), direction = "both", trace = 1)
summary(step.fit)
step.fit$anova


# create regression model 
lm.fit.stepwise <- lm(Crime ~ Po1+Ineq+Ed+M+Prob+U2, data = data)


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
lasso <- glmnet(predictors, response, alpha = 1)
# plotmo
plot_glmnet(lasso)




# find best lambda value
lasso.fit <- cv.glmnet(predictors, response, alpha = 1)
plot(lasso.fit)

# Df is the number of nonzero coefficients
# %Dev is the % of deviance explained
# Lambda is the lambda valued tried
print(lasso)
print(lasso.fit)

# find which variables to use 
coef(lasso.fit, s = lasso.fit$lambda.min)
coef(lasso.fit, s = lasso.fit$lambda.1se)


###   HOW TO INTERPRET ANOVA
###   https://www.graphpad.com/guides/prism/7/curve-fitting/reg_howtheftestworks.htm?toc=0&printWindow
###
###   "It is conventional to list the models from smallest to largest, but this is up to the user."
###

# regression model for lambda.min
lasso.fit.lambdamin <- lm(Crime ~ M+So+Ed+Po1+LF+M.F+NW+U1+U2+Ineq+Prob, data = data)
summary(lasso.fit.lambdamin)


# regression model for lambda.1se
lasso.fit.1se <- lm(Crime ~ M+So+Ed+Po1+M.F+NW+U2+Ineq+Prob, data = data)
summary(lasso.fit.1se)



# model: ridge regression
elastic <- glmnet(predictors, response, alpha = 0.5)
# plotmo
plot_glmnet(elastic)

# find best lambda value
elastic.fit <- cv.glmnet(predictors, response, alpha = 0.5)
plot(elastic.fit)

# Df is the number of nonzero coefficients
# %Dev is the % of deviance explained
# Lambda is the lambda valued tried
print(elastic)
print(elastic.fit)

# find which variables to use 
coef(elastic.fit, s = elastic.fit$lambda.min)
coef(elastic.fit, s = elastic.fit$lambda.1se)

# regression model for lambda.min
elastic.fit.lambdamin <- lm(Crime ~ M+So+Ed+Po1+Po2+M.F+Pop+NW+U1+U2+Wealth+Ineq+Prob, data = data)
summary(elastic.fit.lambdamin)


# regression model for lambda.1se
elastic.fit.1se <- lm(Crime ~ M+Po1+Po2+M.F+NW+Ineq+Prob, data = data)
summary(elastic.fit.1se)



########################################################################################
################### TRY TO NORMALIZED DATE FOR STEPWISE BEFORE ANOVA ###################
########################################################################################
# compare stepwise, lasso and elastic
# step = 6 predictors, elastic1se = 7 predictors, lasso1se = 9 predictors
# lassolamb = 11 predictors, elasticlam = 13 predictors
anova(lm.fit.stepwise, elastic.fit.1se, lasso.fit.1se, lasso.fit.lambdamin, elastic.fit.lambdamin)




#### QUESTION 12.2

library(FrF2)

house.features <- c("large yard", "solar roof", "garage", "pool", "school nearby",
                    "high speed internet", "guest room", "energy efficient", 
                    "two bathrooms", "cable TV")

FrF2(nruns = 16, factor.names = house.features)
