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
y<- unlist(y)
# extract only data related to the predicted temperature
y <- y[1:length(data.vc)]
# add line for predicted values on previous graph
lines(x, y, col = "blue")

