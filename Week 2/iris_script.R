setwd("C:/Users/ROMEST/Downloads/Homework/week 2")

data <- read.table("iris.txt", header = T)
levels(data[,5]) <- c("3", "2", "1")


#######
######  REMEMBER TO SCALE THE DATA
######

set.seed(888)
# BASIC ALGORITHM
# kmeans clustering
kmean.fit <- kmeans(data[,-5], centers = 3, nstart = 25)
# show cluster
kmean.fit$cluster


par(mfrow=c(2,2), mar=c(5,4,2,2))
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



set.seed(111)
# automate trying different number of clusters (k)
accuracy <- 0
for (k in 1:5){
  # kmeans clustering
  kmean.fit <- kmeans(data[,-5], centers = k, nstart = 25)
  # find accuracy
  accuracy[k] <- sum(kmean.fit$cluster == data[,5])/nrow(data)
}
accuracy

