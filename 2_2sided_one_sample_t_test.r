# Data for Risk A and Risk B
risk_a <- c(8.0, 8.4, 8.0, 6.4, 8.6, 7.7, 7.7, 5.6, 5.6, 6.2)
risk_b <- c(5.6, 7.4, 7.3, 6.4, 7.5, 6.1, 6.6, 6.0, 5.5, 5.5)

# Define the hypothesized population mean under H0
mu_h0 <- 7

# Perform a one-sample t-test
t_test_result <- t.test(risk_a, mu = mu_h0, alternative = "two.sided")

# Print the results
cat("One-Sample T-Test Results for Risk A:\n")
cat("--------------------------------------------------\n")
cat("Sample Mean:", mean(risk_a), "\n")
cat("Hypothesized Mean under H0:", mu_h0, "\n")
cat("t-Statistic:", t_test_result$statistic, "\n")
cat("Degrees of Freedom:", t_test_result$parameter, "\n")
cat("P-Value:", t_test_result$p.value, "\n")
cat("--------------------------------------------------\n")

# Check if the null hypothesis is rejected at the 0.05 significance level
if (t_test_result$p.value < 0.05) {
  cat("Conclusion: Reject the null hypothesis. There is a significant difference in the mean of Risk A.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. There is no significant difference in the mean of Risk A.\n")
}

t_test_result
