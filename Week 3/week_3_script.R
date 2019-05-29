setwd("C:/Users/ROMEST/Downloads/homework/Week 3")
library("smooth")

data <- read.table("temps.txt", header = T)

# create vector of data by unlisting the data frame
data.vc <- unlist(data[,-1])

# plot the data as line chart
plot(data.vc, type = "l")

# model
HW.fit <- es(data.vc, model = "AAM")

HW.fit[4]

# vecotr x with number of data points
x <- 1:length(data.vc)
# y with predicted data
y <- HW.fit[4]
# unlist
y<- unlist(y)
# extract only data related to the predicted temperature
y <- y[1:length(data.vc)]
# add line for predicted values on previous graph
lines(x, y, col = "blue")


# apply CUSUM to predicted values

# find avg temperature
# avgtemp <- mean(y)
# 
# 
# # initialize variable
# st <- 0
# C <- 3
# # looping for cusum
# # St= max{0,St???1+ (xt????????C)}
# for (i in 1:length(y)){
#   # first loop
#   if (i == 1) {
#     # average over the years (data$avg)
#     if (0 + (y[i] - avgtemp - C) > 0){
#       st[i] <- 0 + (y[i] - avgtemp - C)
#     } else {
#       st[i] <- 0
#     }
#   } else {
#     if (st[i-1] + (y[i] - avgtemp - C) > 0){
#       st[i] <- st[i-1] + (y[i] - avgtemp - C)
#     } else {
#       st[i] <- 0
#     }
#   }
# }
# 
# plot(st, type = "l")
# # find data point in data set associated with max value of cusum
# y[which.max(st)]
# 
# 
# split.data <- split(y, ceiling(seq_along(y)/122))
