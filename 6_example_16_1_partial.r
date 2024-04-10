# Define the data and parameters
B = c(27,82,115,126,155,161,243,294,340,384,457,680,855,877,974,
      1193,1340,1884,2558,3476)
B_t_50 = B[-1]
theta_hat = mean(B_t_50) - 50

# Define the empirical cumulative distribution function (ECDF)
ecdf_function <- ecdf(B_t_50)

# Create a plot of the ECDF
plot(ecdf_function, main="ECDF vs Exponential CDF", xlab="x", ylab="Cumulative Probability", col="blue")

# Define the exponential distribution function
B_dist = function(x) {
  1 - exp(-(x - 50) / theta_hat)
}

# Plot the exponential distribution function
curve(B_dist(x), from = min(B_t_50), to = max(B_t_50), col = "red", add = TRUE)

# Add a legend
legend("bottomright", legend=c("ECDF", "Exponential CDF"), col=c("blue", "red"), lty=1)


# Calculate the difference between the ECDF and the exponential CDF
x_values <- seq(min(B_t_50), max(B_t_50), length.out = 100)
ecdf_values <- ecdf_function(x_values)
exponential_cdf_values <- pexp(x_values - 50, rate = 1 / theta_hat)
difference_values <- ecdf_values - exponential_cdf_values

# Create a plot of the difference
plot(x_values, difference_values, type = "l", col = "blue",
     main = "Difference between ECDF and Exponential CDF",
     xlab = "x", ylab = "Difference")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)

# Add labels
legend("topright", legend = "Difference", col = "blue", lty = 1)


# Calculate the ECDF values for the data
x_values_data <- sort(B_t_50)

# Calculate the exponential CDF values for the same x-values
exponential_cdf_values <- pexp(x_values_data - 50, rate = 1 / theta_hat)
empirical_values <- seq(1/19, 19/19, by = 1/19)

# Create a scatter plot
plot(empirical_values, exponential_cdf_values,
     xlab = "Fn_star", ylab = "F_star",
     main = "p-p plot",
     col = "blue", pch = 19)

# Add a reference line for y = x
abline(0, 1, col = "red")



