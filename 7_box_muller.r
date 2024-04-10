# Number of random normals to generate
n <- 100000

# Generate n/2 uniform random numbers for U1 and U2
U1 <- runif(n / 2)
U2 <- runif(n / 2)

# Box-Muller Transform
Z1 <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)
Z2 <- sqrt(-2 * log(U1)) * sin(2 * pi * U2)

# Combine the two sets of random normals
Z <- c(Z1, Z2)

# Print the generated random normals
Z

hist(Z)
hist(rnorm(n), col=rgb(1,0,0,0.5),add=TRUE)
