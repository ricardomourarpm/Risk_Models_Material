# Data for Risk A and Risk B (assuming equal sample sizes)
risk_a <- c(8.0, 8.4, 8.0, 6.4, 8.6, 7.7, 7.7, 5.6, 5.6, 6.2)
risk_b <- c(5.6, 7.4, 7.3, 6.4, 7.5, 6.1, 6.6, 6.0, 5.5, 5.5)

# Perform a two-sample F-test for variances
var_test_result <- var.test(risk_a, risk_b)

# Print the results
cat("Two-Sample F-Test Results for Equality of Variances:\n")
cat("--------------------------------------------------\n")
cat("Test Statistic (F):", var_test_result$statistic, "\n")
cat("Degrees of Freedom (Num, Den):", var_test_result$parameter[1], ",", var_test_result$parameter[2], "\n")
cat("P-Value:", var_test_result$p.value, "\n")
cat("--------------------------------------------------\n")

# Check if the null hypothesis is rejected at the 0.05 significance level
if (var_test_result$p.value < 0.05) {
  cat("Conclusion: Reject the null hypothesis. There is evidence of unequal variances (σ_A^2/σ_B^2 ≠ 1).\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. There is no significant evidence of unequal variances (σ_A^2/σ_B^2 = 1).\n")
}

var_test_result
