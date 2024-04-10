data = read.table("C:\\Users\\ricar\\OneDrive - FCT NOVA\\ISEG\\Risk Models\\bootstrap files\\Verizon1_ilec.prn")

hist(data$V1, breaks = 150)

n<-length(data$V1)

qqnorm(data$V1)
qqline(data, col='red')

sim = 1000

means = c(rep(NA,sim))

for (i in 1:sim){
  b_sample <- sample(data$V1, n, replace = TRUE)
  means[i] = mean(b_sample)
}

mean(means)
mean(data$V1)
sd(data$V1)/sqrt(n)
sd(means)
library(moments)
skewness(means) #near zero
kurtosis(means) #near three


hist(means,50,probability = TRUE)
lines(density(means))

# Using library boot
library(boot)
means_2 = function(x,i){
  data = x[i];
  return(mean(data))
}
boot_from_boot = boot(data$V1, means_2, R=1001)
boot_from_boot


