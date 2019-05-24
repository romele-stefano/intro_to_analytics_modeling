setwd("C:/Users/ROMEST/Downloads/Homework/week 2")
setwd("C:/Users/romel/Downloads/week 2")

data <- read.table("iris.txt", header = T)
levels(data[,5]) <- c("3", "2", "1")


# QUESTION 4.2

set.seed(888)
# BASIC ALGORITHM

# kmeans clustering
kmean.fit <- kmeans(data[,-5], centers = 3, nstart = 25)
# show cluster
# kmean.fit$cluster


par(mfrow =c (2,2), mar = c(5,4,2,2))
# plot the prediction
plot(data[c(1,2)], col = kmean.fit$cluster, main = "Cluster")
# plot real data
plot(data[c(1,2)], col = data[,5], main = "Real Data")
# plot the prediction
plot(data[c(3,4)], col = kmean.fit$cluster, main = "Cluster")
# plot real data
plot(data[c(3,4)], col = data[,5], main = "Real Data")



# Iris Setosa = 1, Iris Versicolor = 2, Iris Virginica = 3
confusion <- table(kmean.fit$cluster, data[,5])
# find accuracy
sum(kmean.fit$cluster == data[,5])/nrow(data)

# SEE https://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(data[,-5],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# Different combinations of k
set.seed(111)
# automate trying different number of clusters (k)
par(mfrow = c(2,3), mar = c(5,4,2,2))
accuracy <- 0
for (k in 1:6){
  # kmeans clustering
  kmean.fit <- kmeans(data[,-5], centers = k, nstart = 25)
  # find accuracy
  accuracy[k] <- sum(kmean.fit$cluster == data[,5])/nrow(data)
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 1:15) wss[i] <- sum(kmeans(data[,-5],
                                       centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}
accuracy

# In general, we have the best results with k = 3
# Let's try using only some predictors

set.seed(888)
# BASIC ALGORITHM FOR SEPAL LENGTH AND WIDTH

# kmeans clustering
kmean.fit <- kmeans(data[,-c(3:5)], centers = 3, nstart = 25)
# show cluster
# kmean.fit$cluster


par(mfrow =c (2,2), mar = c(5,4,2,2))
# plot the prediction
plot(data[c(1,2)], col = kmean.fit$cluster, main = "Cluster")
# plot real data
plot(data[c(1,2)], col = data[,5], main = "Real Data")
# plot the prediction
plot(data[c(3,4)], col = kmean.fit$cluster, main = "Cluster")
# plot real data
plot(data[c(3,4)], col = data[,5], main = "Real Data")



# Iris Setosa = 1, Iris Versicolor = 2, Iris Virginica = 3
confusion <- table(kmean.fit$cluster, data[,5])
# find accuracy
sum(kmean.fit$cluster == data[,5])/nrow(data)



set.seed(888)
# BASIC ALGORITHM FOR PETAL LENGTH AND WIDTH

# kmeans clustering
kmean.fit <- kmeans(data[,-c(1,2,5)], centers = 3, nstart = 25)
# show cluster
# kmean.fit$cluster


par(mfrow =c (2,2), mar = c(5,4,2,2))
# plot the prediction
plot(data[c(1,2)], col = kmean.fit$cluster, main = "Cluster")
# plot real data
plot(data[c(1,2)], col = data[,5], main = "Real Data")
# plot the prediction
plot(data[c(3,4)], col = kmean.fit$cluster, main = "Cluster")
# plot real data
plot(data[c(3,4)], col = data[,5], main = "Real Data")


# Iris Setosa = 1, Iris Versicolor = 2, Iris Virginica = 3
confusion <- table(kmean.fit$cluster, data[,5])
# find accuracy
sum(kmean.fit$cluster == data[,5])/nrow(data)








# CRIME DATA

# Variable	 	Description
# M		percentage of males aged 14-24 in total state population
# So		indicator variable for a southern state
# Ed		mean years of schooling of the population aged 25 years or over
# Po1		per capita expenditure on police protection in 1960
# Po2		per capita expenditure on police protection in 1959
# LF		labour force participation rate of civilian urban males in the age-group 14-24
# M.F		number of males per 100 females
# Pop		state population in 1960 in hundred thousands
# NW		percentage of nonwhites in the population
# U1		unemployment rate of urban males 14-24
# U2		unemployment rate of urban males 35-39
# Wealth		wealth: median value of transferable assets or family income
# Ineq		income inequality: percentage of families earning below half the median income
# Prob		probability of imprisonment: ratio of number of commitments to number of offenses
# Time		average time in months served by offenders in state prisons before their first release
# Crime		crime rate: number of offenses per 100,000 population in 1960

library("outliers")
data <- read.table("uscrime.txt", header = TRUE)
crimes <- data$Crime

outlier <- grubbs.test(crimes, type = 11)
# with type = 11
# H0 - NULL HYPOTESiS - No Outlier
# H1 - alternative hypotesis - outlier present

# the p-value indicates there is no evidence whatsoever that any of  data are outliers because the p-value is 1

# Grubbs test for two opposite outliers
# 
# data:  crimes
# G = 4.26880, U = 0.78103, p-value = 1
# alternative hypothesis: 342 and 1993 are outliers


# dive into the data set
attach(data)
par(mfrow =c (1,2), mar = c(5,4,2,2))
plot(Crime, Wealth)
points(data[which.max(data$Crime),]$Crime, data[which.max(data$Crime),]$Wealth, col = "red")
points(data[which.min(data$Crime),]$Crime, data[which.min(data$Crime),]$Wealth, col = "red")
plot(Crime, Ineq)
points(data[which.max(data$Crime),]$Crime, data[which.max(data$Crime),]$Ineq, col = "red")
points(data[which.min(data$Crime),]$Crime, data[which.min(data$Crime),]$Ineq, col = "red")
par(mfrow =c (1,2), mar = c(5,4,2,2))
plot(Crime, NW)
points(data[which.max(data$Crime),]$Crime, data[which.max(data$Crime),]$NW, col = "red")
points(data[which.min(data$Crime),]$Crime, data[which.min(data$Crime),]$NW, col = "red")
plot(Crime, Ed)
points(data[which.max(data$Crime),]$Crime, data[which.max(data$Crime),]$Ed, col = "red")
points(data[which.min(data$Crime),]$Crime, data[which.min(data$Crime),]$Ed, col = "red")
par(mfrow =c (1,2), mar = c(5,4,2,2))
plot(Crime, U1)
points(data[which.max(data$Crime),]$Crime, data[which.max(data$Crime),]$U1, col = "red")
points(data[which.min(data$Crime),]$Crime, data[which.min(data$Crime),]$U1, col = "red")
plot(Crime, U2)
points(data[which.max(data$Crime),]$Crime, data[which.max(data$Crime),]$U2, col = "red")
points(data[which.min(data$Crime),]$Crime, data[which.min(data$Crime),]$U2, col = "red")




##### QUESTION 6.2 A

set.seed(888)
data <- read.table("temps.txt", header = T)
# add new column to data frame
# find the average value over rows (1 = rows) for each day (this will be our X)
data$avg <- apply(data[,-1], 1, mean)

# find global average (this will be our "mu")
avg <- mean(data[,22])

# initialize variable
st <- 0
C <- 3
# looping for cusum
# St= max{0,St???1+ (xt????????C)}
for (i in 1:nrow(data)){
  # first loop
  if (i == 1) {
    # average over the years (data$avg)
    if (0 + (data$avg[i] - avg - C) > 0){
      st[i] <- 0 + (data$avg[i] - avg - C)
    } else {
      st[i] <- 0
    }
  } else {
    if (st[i-1] + (data$avg[i] - avg - C) > 0){
      st[i] <- st[i-1] + (data$avg[i] - avg - C)
    } else {
      st[i] <- 0
    }
  }
}

plot(st)
# find data point in data set associated with max value of cusum
data[which.max(st), ]



##### QUESTION 6.2 B

set.seed(888)
data <- read.table("temps.txt", header = T)
day <- data[,1]
data <- data[,-1]

# initialize parameter
C <- 3
# initialized empty matrix for storing data
st <- matrix(0, nrow = nrow(data), ncol = ncol(data))
# loop over columns 
for (j in 1:ncol(data)){
  # row avg
  avgr <- mean(data[,j])
  # loop over rows
  for (i in 1:nrow(data)){
    # first loop
    if (i == 1) {
      if (0 + (data[i,j] - avgr - C) > 0){
        st[i, j] <- 0 + (data[i,j] - avgr - C)
      } else {
        st[i, j] <- 0
      }
    } else {
      if (st[i-1] + (data[i,j]- avgr - C) > 0){
        st[i, j] <- st[i-1] + (data[i,j] - avgr - C)
      } else {
        st[i, j] <- 0
      }
    }
  }
}

# transform intro data frame my cusum values
st <- as.data.frame(st)

# plotting some years
plot(st[,1], ylab = "CUSUM", main = "Evolution of CUSUM over time")
points(st[,10], col = "blue")
points(st[,20], col = "red")
legend("topright", legend = c("1996", "2005", "2015"), col =c("black", "blue", "red"), lty = 1:2, cex = 0.8)
