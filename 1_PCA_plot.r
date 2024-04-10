# Create the dataset
x_1 <- c(16, 12, 13, 11, 10, 9, 8, 7, 5, 3, 2, 0)
x_2 <- c(8, 10, 6, 2, 8, -1, 4, 6, -3, -1, -3, 0)

# Calculate the means of x_1 and x_2
mean_x_1 <- mean(x_1)
mean_x_2 <- mean(x_2)

# Combine the data into a matrix
data_matrix <- cbind(x_1, x_2)

# Compute the eigenvectors
eigen_result <- eigen(cov(data_matrix))  # Compute eigenvectors of the covariance matrix

# Extract the eigenvectors
eigenvector_1 <- eigen_result$vectors[, 1]*5
eigenvector_2 <- eigen_result$vectors[, 2]*5

# Create a scatter plot
plot(x_1, x_2, 
     type = "p",        # Use points for the plot
     pch = 16,          # Set the point shape to filled circles
     col = "blue",      # Set the point color
     xlim = c(-5, 20),  # Set the x-axis limits
     ylim = c(-5, 15),  # Set the y-axis limits
     xlab = "X_1",      # Label for the x-axis
     ylab = "X_2",      # Label for the y-axis
     main = "Scatter Plot with Centered Eigenvectors")  # Title of the plot

# Add labels (letters) to each point
text(x_1, x_2, labels = labels, pos = 3)




# Add axis lines in origin
abline(h = 0, v = 0, col = "gray")

################################


# Plot the centered eigenvectors as lines with arrows at the end
arrows(mean_x_1, mean_x_2, 
       mean_x_1 - eigenvector_1[1], mean_x_2 - eigenvector_1[2], 
       col = "red", lwd = 2, length = 0.1)
arrows(mean_x_1, mean_x_2, 
       mean_x_1 - eigenvector_2[1], mean_x_2 - eigenvector_2[2], 
       col = "green", lwd = 2, length = 0.1)


#################################

# Perform Principal Component Analysis (PCA)
pca_result <- prcomp(data_matrix, scale = TRUE)

# Project the original points onto the first two principal components
projected_points <- pca_result$x[, 1:2]

# Create a scatter plot in PCA coordinates
plot(projected_points, 
     type = "p",        # Use points for the plot
     pch = 16,          # Set the point shape to filled circles
     col = "blue",      # Set the point color
     xlim = c(-5, 5),   # Set the x-axis limits
     ylim = c(-5, 5),   # Set the y-axis limits
     xlab = "PC1",      # Label for the x-axis (PC1)
     ylab = "PC2",      # Label for the y-axis (PC2)
     main = "PCA Plot of Dataset")  # Title of the plot

# Add labels (letters) to each point
text(projected_points[, 1], projected_points[, 2], labels = labels, pos = 3)

# Add axis lines in origin
abline(h = 0, v = 0, col = "gray")



###################################

