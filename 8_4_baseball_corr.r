baseball = read.table("C:\\Users\\ricar\\OneDrive - FCT NOVA\\ISEG\\Risk Models\\bootstrap files\\baseball.prn", header=TRUE)

x1=baseball$Salary; x2=baseball$Average;

plot(x2,x1)

mean1 = mean(x1);mean2 = mean(x2);
sd1 = sd(x1); sd2 = sd(x2);

corr_x1x2=cor(x1,x2); n = length(x1)

mean1; mean2; corr_x1x2; sd1; sd2;

# bootstrap

sims = 10000; corrs = rep(NA, sims);
for(i in 1:sims){
  j = sample(1:n, replace = TRUE)
  x1b = x1[j]; x2b = x2[j];
  corrs[i] = cor(x1b,x2b)
}

mean_corr = mean(corrs); sd_corr = sd(corrs);
sk_corr = (1/(sims-1))*sum((corrs-mean_corr)^3)/(sd_corr^3)
kurt_corr = (1/(sims-1))*sum((corrs-mean_corr)^4)/(sd_corr^4)

mean_corr; sd_corr; sk_corr; kurt_corr;

ks.test(corrs, "pnorm", mean = corr_x1x2, sd = sd_corr)

expected_quantiles <- qnorm(seq(0.0001, 0.9999, by = 0.0001), mean = corr_x1x2, sd = sd_corr)

qqplot(corrs, expected_quantiles)
abline(a = 0, b = 1, col = "red")

# Using boot

x = cbind(x1,x2)
require(boot)
corr(x) #from boot

b_correlation = function(x,i){
  data = x[i,]; return(corr(data))
}

out = boot(x, b_correlation, R = sims)
out