setwd("C:/Users/ROMEST/Downloads")
library("kernlab")

data <- read.table("credit_card_data-headers.txt", header = T)
datamatrix <- as.matrix(data)

set.seed(888)

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
  svm.fit <- ksvm(datamatrix[,1:10], datamatrix[,11], type="C-svc", kernel = "vanilladot", C = 0.1^i, scaled = TRUE)
  
  # predict
  pred <- predict(svm.fit, data[,1:10])
  res[i] <- sum(pred == data[,11]) / nrow(data)
  
}