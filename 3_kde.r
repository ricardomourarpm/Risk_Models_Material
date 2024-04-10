y = c(1,1.3,1.5,2.1,2.8)
p_y = c(1/8,1/8,2/8,3/8,1/8)

k_y = dnorm(2, y, 0.3)
f_ = sum(p_y*k_y)

f_

K_y = pnorm(2,y,0.3)

F_ = sum(p_y*K_y)
F_

# Generate a sequence of y-values for the density estimation
y_values <- seq(min(y) - 1, max(y) + 1, length.out = 100)

# Calculate the KDE at each y-value using the Gaussian kernel
kde_values <- sapply(y_values, function(x) sum(p_y * dnorm(x, y, 0.3)))
kde_values_2 <- sapply(y_values, function(x) sum(p_y * dnorm(x, y, 0.4)))

# Calculate the CDF at each y-value using the standard normal CDF
cdf_values <- sapply(y_values, function(x) sum(p_y * pnorm(x, y, 0.3)))
cdf_values_2 <- sapply(y_values, function(x) sum(p_y * pnorm(x, y, 0.4)))

# Plot the Kernel Density Estimate (KDE)
plot(y_values, kde_values, type = "l", col = "blue", lwd = 2, main = "Kernel Density Estimate (KDE) and CDF",
     xlab = "y-values", ylab = "Density", xlim = c(min(y_values), max(y_values)))
lines(y, rep(0, length(y)), type = "p", pch = 19, col = "red")
par(new = TRUE)
plot(y_values, kde_values_2, type = "l", col = "black", lwd = 2, main = "Kernel Density Estimate (KDE) and CDF",
     xlab = "y-values", ylab = "Density", xlim = c(min(y_values), max(y_values)))


# Add the CDF curve to the plot
par(new = TRUE)
plot(y_values, cdf_values, type = "l", col = "green", lwd = 2, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(4)
mtext("CDF", side = 4, line = 3)
# Add the CDF curve to the plot
par(new = TRUE)
plot(y_values, cdf_values_2, type = "l", col = "red", lwd = 2, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(4)
mtext("CDF", side = 4, line = 3)
