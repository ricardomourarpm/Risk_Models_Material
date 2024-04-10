# Data for Risk A and Risk B
risk_a <- c(8.0, 8.4, 8.0, 6.4, 8.6, 7.7, 7.7, 5.6, 5.6, 6.2)
risk_b <- c(5.6, 7.4, 7.3, 6.4, 7.5, 6.1, 6.6, 6.0, 5.5, 5.5)

# Perform a two-sample t-test assuming unequal variances (you could add var.equal = F)
t_test_result <- t.test(risk_a, risk_b, alternative = "greater")

# Print the results
cat("Two-Sample T-Test Results (Assuming Unequal Variances):\n")
cat("--------------------------------------------------\n")
cat("Sample Mean of Risk A:", mean(risk_a), "\n")
cat("Sample Mean of Risk B:", mean(risk_b), "\n")
cat("t-Statistic:", t_test_result$statistic, "\n")
cat("Degrees of Freedom (approximate):", t_test_result$parameter, "\n")
cat("P-Value:", t_test_result$p.value, "\n")
cat("--------------------------------------------------\n")

# Check if the null hypothesis is rejected at the 0.05 significance level
if (t_test_result$p.value < 0.05) {
  cat("Conclusion: Reject the null hypothesis. There is a significant difference in the means, and the mean of Risk A is greater than the mean of Risk B.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. There is no significant difference in the means, or the mean of Risk A is not greater than the mean of Risk B.\n")
}
t_test_result
