# Number of "YES" responses (satisfied) and total number of responses
n_yes <- 510
n_total <- 1000

# Perform a one-sample proportion z-test
prop_test_result <- prop.test(n_yes, n_total, p = 0.5, alternative = "greater",correct = FALSE)

# Set the significance level (alpha)
alpha <- 0.05

# Print the results
cat("One-Sample Proportion Z-Test Results:\n")
cat("--------------------------------------------------\n")
cat("Sample Proportion (p-hat):", prop_test_result$estimate, "\n")
cat("Null Hypothesis Proportion (p0):", 0.5, "\n")
cat("Test Statistic (Z):", prop_test_result$statistic, "\n")
cat("P-Value:", prop_test_result$p.value, "\n")
cat("Significance Level (alpha):", alpha, "\n")
cat("Confidence Interval:", prop_test_result$conf.int, "\n")
cat("--------------------------------------------------\n")

# Check if the null hypothesis is rejected at the specified significance level
if (prop_test_result$p.value < alpha) {
  cat("Conclusion: Reject the null hypothesis. There is evidence that more than 50% of insured people are satisfied.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. There is no significant evidence that more than 50% of insured people are satisfied.\n")
}

prop_test_result
