# Install and load the "actuar" library (if not already installed)
library(actuar)

# Define the data
data <- read.csv("C:/Users/ricar/OneDrive - FCT NOVA/ISEG/Risk Models/Scripts/Data_C.csv")

x = c(data$Linf,1000000)
x_1 = c(data$Linf, Inf)
y = data$Number

# Funtion ogive
Fn <- ogive(x,y)

# Plot the ogive (CDF)
Fn
plot(Fn)

cbind(Fn(1000),Fn(7500),Fn(300000),Fn(302000),Fn(1000000))

# Funtion ogive
Fn <- ogive(x_1,y)

# Plot the ogive (CDF)
Fn
plot(Fn, xlim = c(0, 1000000)) # Oh! No!

cbind(Fn(1000),Fn(7500),Fn(300000),Fn(302000),Fn(1000000))



######### without ogive


# Calculate the cumulative distribution function (CDF) manually
CumulativeFreq <- cumsum(y) / sum(y)
leftb = x[-length(x)]
rightb = x[-1]
leftFn = c(0, CumulativeFreq[-length(CumulativeFreq)])
rightFn = CumulativeFreq

intercept = (rightb*leftFn-leftb*rightFn)/(rightb-leftb)
slope = (rightFn-leftFn)/(rightb-leftb)

Fn = c(0,rightFn)

# Create an ogive (CDF) plot manually
plot(x, Fn, type = "l", main = "Ogive (CDF) Plot", xlab = "Payment", ylab = "Cumulative Frequency")
points(x, Fn, pch=16)

#The empirical density is given by the slope

#To plot a histogram we need to "simulate" the observations
# As Only the number of observations in each interval matters,
# we choose an arbitrarily value inside each interval and define array z

# Create a histogram
hist_data <- rep(data$Linf, data$Number)
hist(hist_data, main = "Histogram of Payments", xlab = "Payments", ylab = "Frequency", breaks = x)
hist(hist_data, main = "Histogram of Payments", xlab = "Payments", ylab = "Frequency", breaks = data$Linf)
hist(hist_data, main = "Histogram of Payments", xlab = "Payments", ylab = "Frequency", breaks = x, xlim = c(0,130000))




