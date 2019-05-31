setwd("C:/Users/ROMEST/Downloads/homework/Week 3")
library("smooth")

set.seed(888)
data <- read.table("temps.txt", header = T)

# create vector of data by unlisting the data frame
data.vc <- unlist(data[,-1])

# plot the data as line chart
plot(data.vc, type = "l")

# model
HW.fit <- es(data.vc, model = "AAM")
# the model gives "beta" value of 0.000. "Beta" is the trand smoothing function
# a value near 0 means there is no significant trend, suggesting there is not a 
# significant increase or decrease over the years.

HW.fit[4]

# vecotr x with number of data points
x <- 1:length(data.vc)
# y with predicted data
y <- HW.fit[4]
# unlist
y <- unlist(y)
# extract only data related to the predicted temperature
y <- y[1:length(data.vc)]
# add line for predicted values on previous graph
lines(x, y, col = "blue")

# split HW.fit into years
s <- as.vector(y)
splitted <- split(s, ceiling(seq_along(s)/123))

y.dataframe <- as.data.frame(splitted)
y.dataframewithday <- cbind(data$DAY, y.dataframe)



# initialize parameter
C <- 3
# initialized empty matrix for storing data
st <- matrix(0, nrow = nrow(y.dataframe), ncol = ncol(y.dataframe))
# loop over columns 
for (j in 1:ncol(y.dataframe)){
  # row avg
  avgr <- mean(y.dataframe[,j])
  # loop over rows
  for (i in 1:nrow(y.dataframe)){
    # first loop
    if (i == 1) {
      if (0 + (y.dataframe[i,j] - avgr - C) > 0){
        st[i, j] <- 0 + (y.dataframe[i,j] - avgr - C)
      } else {
        st[i, j] <- 0
      }
    } else {
      if (st[i-1] + (y.dataframe[i,j]- avgr - C) > 0){
        st[i, j] <- st[i-1] + (y.dataframe[i,j] - avgr - C)
      } else {
        st[i, j] <- 0
      }
    }
  }
}

# transform intro data frame my cusum values
st <- as.data.frame(st)

# find max for each year
for (i in 1:ncol(st)){
  M[i] <- which.max(st[,i])
}

format(Sys.Date(), format = "%d-%B")
Sys.setlocale("LC_TIME", "C")
D <- y.dataframewithday[M,1]
d <- as.character(D)
Y <- 1996:2015
plot(Y, D, type = "l", xaxt= "n", yaxt = "n")
axis(1, Y) 
axis.Date(2, at = unique(d), format = "%d-%B")



## REGRESSION

set.seed(888)
data <- read.table("uscrime.txt", header = T)

# train the model on all predictors
glm.fit <- glm(Crime ~ ., family = "gaussian", data)

# data point to use for prediction
p <- cbind(14,0,10,12,15.5,0.640,94,150,1.1,0.120,3.6,3200,20.1,0.04,39)
p <- as.data.frame(p)
colnames(p) <- c("M","So","Ed","Po1","Po2","LF","M.F","Pop","NW","U1","U2","Wealth","Ineq","Prob","Time")


# prediction
prediction <- predict(glm.fit, newdata = p)


# TO DO
# Create second model with less predictors (based on p-values) and compare AIC

