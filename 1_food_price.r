data = read.csv("C:/Users/ricar/OneDrive - FCT NOVA/ISEG/Risk Models/data/food_price_index.csv")
data

attach(data) # Attach the data frame to the search path
# Now, you can directly access variables in the data frame without specifying the data frame's name

x = cbind(Bread, Burger, Milk, Oranges, Tomatoes)
apply(x,2,mean) #2 This argument specifies that you want to apply the function to columns. If you use 1 instead of 2, it would apply the function to rows.
apply(x,2,sd)
cor(x)

out1=prcomp(x, center=T)
out1

summary(out1)

out2=prcomp(x, center=T,scale=T)
out2

summary(out2)

# Keiser Criterion

lambda=out1$sdev^2; lambda
mean(lambda)

# Scree Plot
lambda=out2$sdev^2
plot(lambda,type="b")
diff(lambda,lag=1,differences=2)

require(paran)# package paran must be installed before
paran(x,iterations=1000,graph=T)

cor(x,out2$x[,c(1:2)])    # Loadings

cor(x,out1$x[,c(1:2)])    # Loadings


# Load the ggplot2 package if not already loaded
library(ggplot2)

# Create a data frame containing PC1, PC2, and City columns
pc_data <- data.frame(PC1 = out2$x[, 1], PC2 = out2$x[, 2], City = data$City)

# Create a scatter plot
ggplot(pc_data, aes(x = PC1, y = PC2, label = City)) +
  geom_point() +
  geom_text(nudge_x = 0.1, nudge_y = 0.1) +  # Add labels with a slight nudge for better visibility
  labs(x = "PC1", y = "PC2", title = "PCA Scatter Plot") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black")  # Set axis lines to black
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +  # Add horizontal line (y-axis)
  geom_vline(xintercept = 0, linetype = "dotted", color = "blue")  # Add vertical line (x-axis)


