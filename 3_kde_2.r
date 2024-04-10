# Given data
y <- c(1.0, 1.3, 1.5, 2.1, 2.8)
s <- c(1, 1, 2, 3, 1)
n <- sum(s)

# Calculate probabilities
p_y <- s / n

# Define x-values for density estimation
x <- seq(0, 4, by = 0.025)

# Initialize an empty vector to store density estimates
fx <- rep(NA, length(x))

# Uniform kernel
b <- 0.5
LU <- y - b
UU <- y + b

for (i in 1:length(x)) {
  # Calculate density estimate using uniform kernel
  fx[i] <- sum(p_y * dunif(x[i], LU, UU))
}

# Plot uniform kernel density estimate
label_uniform <- paste("Uniform Kernel Density Estimate (b =", b, ")", sep = "")
plot(x, fx, type = "l", main = label_uniform)

# Gamma kernel
alpha <- 50

for (i in 1:length(x)) {
  # Calculate density estimate using gamma kernel
  fx[i] <- sum(p_y * dgamma(x[i], shape = alpha, scale = y / alpha))
}

# Plot gamma kernel density estimate
label_gamma <- paste("Gamma Kernel Density Estimate (alpha =", alpha, ")", sep = "")
plot(x, fx, type = "l", main = label_gamma)
