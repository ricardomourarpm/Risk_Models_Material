ILEC = read.table("C:\\Users\\ricar\\OneDrive - FCT NOVA\\ISEG\\Risk Models\\bootstrap files\\Verizon1_ilec.prn")
CLEC = read.table("C:\\Users\\ricar\\OneDrive - FCT NOVA\\ISEG\\Risk Models\\bootstrap files\\Verizon1_clec.prn")

n1 = length(ILEC$V1)
n2 = length(CLEC$V1)
mean1 = mean(ILEC$V1)
mean2 = mean(CLEC$V1)
sd1 = sd(ILEC$V1)
sd2 = sd(CLEC$V1)

diff_mean = mean1-mean2
diff_mean; mean1; sd1; mean2; sd2;


# Plot density of ILEC data
density_ilec <- density(ILEC$V1)
plot(density_ilec, main = "Density Plot - ILEC")

# Plot density of CLEC data over the ILEC plot
density_clec <- density(CLEC$V1)
lines(density_clec, col = "red")

# bootstrap
sims = 1000; diffs = rep(NA,sims);
for(i in 1:sims){
  x1b = sample(ILEC$V1, replace = TRUE)
  x2b = sample(CLEC$V1, replace = TRUE)
  diffs[i] = mean(x1b)-mean(x2b)
}

#bootstrap results
hist(diffs, probability = TRUE, col = "lightblue", border = "black", main = "Bootstrap Distribution of Mean Differences")
lines(density(diffs), col = "red", lwd = 2)
mean_diff = mean(diffs); sd_diff = sd(diffs)
mean_diff;sd_diff
diff_mean
