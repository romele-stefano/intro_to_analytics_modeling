library('datasets')

data <- read.csv("iris.txt", header = F)
# columns are: Sepal.Length Sepal.Width Petal.Length Petal.Width Species
colnames(data) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
levels(data[,5]) <- c("3", "2", "1")

set.seed(888)
# BASIC ALGORITHM
# kmeans clustering
kmean.fit <- kmeans(iris[,-5], centers = 1)
# show cluster
kmean.fit$cluster


par(mfrow=c(1,2), mar=c(5,4,2,2))
# plot the prediction
plot(data[c(1,2)], col = kmean.fit$cluster)
# plot real data
plot(data[c(1,2)], col = data[,5])

# Iris Setosa = 1, Iris Versicolor = 2, Iris Virginica = 3
confusion <- table(kmean.fit$cluster, data[,5])
# find accuracy
sum(kmean.fit$cluster == data[,5])/nrow(data)


# automate trying different number of clusters (k)
accuracy <- 0
for (k in 1:5){
  # kmeans clustering
  kmean.fit <- kmeans(iris[,-5], centers = k)
  # find accuracy
  accuracy[k] <- sum(kmean.fit$cluster == data[,5])/nrow(data)
  
}

