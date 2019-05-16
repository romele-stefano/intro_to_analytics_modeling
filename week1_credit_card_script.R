setwd("C:/Users/ROMEST/Downloads")
# SVM
library("kernlab")
# KNN
library("kknn")
# k-fold cross validation
library("caret")

data <- read.table("credit_card_data-headers.txt", header = T)
datamatrix <- as.matrix(data)

set.seed(888)


# QUESTION 2.1



# train the model 
# LARGE C = small margin
# SMALL C = large margin
# C = 100 gives an accuracy of 0.8639144
# C = 120 gives an accuracy of 0.8639144
# C = 1 gives an accuracy of 0.8639144
# C = 55550 gives an accuracy of 0.8623853
# C = 100000 gives an accuracy of 0.8639144
svm.fit <- ksvm(datamatrix[,1:10], datamatrix[,11], type="C-svc", kernel = "vanilladot", C = 100000, scaled = TRUE)

# predict
pred <- predict(svm.fit, data[,1:10])
pred
sum(pred == data[,11]) / nrow(data)

# initialized variable
res <- 0


# for loop for automatic testing model with differnt values of C, kernel = "vanilladot"
# C = i * 100 gives values inside res <- 0.8639144 0.8639144 0.8623853 0.8623853 0.8639144
# C = 100^i gives values inside res <- 0.8639144 0.8623853 0.6253823 0.6636086 0.4923547
# C = 0.1^i gives values inside res <- 0.8639144 0.8639144 0.8379205 0.5474006 0.5474006
for (i in 1:5) {
  #train the model
  svm.fit <- ksvm(datamatrix[,1:10], datamatrix[,11], type = "C-svc", kernel = "vanilladot", C = 100^i, scaled = TRUE)
  
  # predict
  pred <- predict(svm.fit, data[,1:10])
  res[i] <- sum(pred == data[,11]) / nrow(data)
  
}

# We use the C = 100^i and select the model with C = 100^1
svm.fit <- ksvm(datamatrix[,1:10], datamatrix[,11], type = "C-svc", kernel = "vanilladot", C = 100, scaled = TRUE)
# predict
pred <- predict(svm.fit, data[,1:10])
res <- sum(pred == data[,11]) / nrow(data)

# Find equation of the SVM above
# calculate a1.am
a <- colSums(svm.fit@xmatrix[[1]] * svm.fit@coef[[1]])
# calculate a0
a0 <- svm.fit@b


# QUESTION 2.1

# Test polynomial kernel
# C = 100^i gives values inside res <- 0.8639144 0.8623853 0.3318043 0.6773700 0.7217125
for (i in 1:5) {
  #train the 
  svm.fit <- ksvm(datamatrix[,1:10], datamatrix[,11], type="C-svc", kernel = "polydot", C = 100^i, scaled = TRUE)
  
  # predict
  pred <- predict(svm.fit, data[,1:10])
  res[i] <- sum(pred == data[,11]) / nrow(data)
  
}





# QUESTION 2.3

# KNN
# create empty matrix to store results of double loop
fit_matrix <- matrix(0, nrow = nrow(data), ncol = 10 )

# loop "i" for testing all data -1 data point each time
# loop "j" for testing different values of k each time
for (i in 1:nrow(data)){
  for (j in 1:10){
    knn.fit = kknn(R1~.,data[-i,],data[i,], k = j, scale = TRUE) # use scaled data
    fit_matrix[i,j] <- fitted(knn.fit)
  }
}

# transform continuous prediction in binary
fit_matrix <- ifelse(fit_matrix > 0.5,1,0)
colnames(fit_matrix) <- c("K=1", "K=2", "K=3", "K=4", "K=5", "K=6", "K=7", "K=8", "K=9", "K=10")
accuracy <- 0
# accuracy with different values of k <- [1] 0.8149847 0.8149847 0.8149847 0.8149847 0.8516820 
# 0.8455657 0.8470948 0.8486239 0.8470948 0.8501529
for (k in 1:10){
  table(data[,11], fit_matrix[,k]) 
  accuracy[k] <- mean(fit_matrix[,k] == data$R1)
}




# QUESTION 3.1.A

# k-folds = 10, k = 10
# taking data set as it is, we have an avg accuracy of  0.8268531
# shuffling the data, we have an avg accuracy of  0.8502331

# k-folds = 10, k = 3
# shuffling the data, we have an avg accuracy of  0.7885781

# k-folds = 10, k = 7
# shuffling the data, we have an avg accuracy of  0.8222378

data_shuffled <- data[sample(nrow(data)),]
# store the results
results <- 0

# set number of folds
k_folds <- 10
folds <- cut(seq(1,nrow(data)),breaks = k_folds, labels = FALSE)
for(i in 1:k_folds){
  # find index in dataset 
  index <- which(folds == i)
  train <- data[-index,] 
  validation <- data[index, ]
  
  #train the model
  knn.fit = kknn(R1~., train, validation[,-11], k = 7, scale = TRUE) # use scaled data
  predicted <- fitted(knn.fit)
  predicted <- ifelse(predicted > 0.5,1,0)
  table(validation$R1, predicted) 
  accuracy[i] <- mean(validation$R1 == predicted)
}

avg <- sum(accuracy)/length(accuracy)




# QUESTION 3.1.B

# create a sample with 70% of observation
s <- sample(1:3, size = nrow(data), prob = c(0.7, 0.15, 0.15), replace = TRUE)
train <- datamatrix[s == 1,]
validation <- datamatrix[s == 2,]
test <- datamatrix[s == 3,]

svm.fit <- ksvm(as.matrix(train[,1:10]), as.matrix(train[,11]), type="C-svc", kernel = "vanilladot", C = 100, scaled = TRUE)

# predict
pred <- predict(svm.fit, validation[,1:10])
pred
sum(pred == validation[,11]) / nrow(data)
