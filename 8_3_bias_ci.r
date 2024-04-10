data = read.table("C:\\Users\\ricar\\OneDrive - FCT NOVA\\ISEG\\Risk Models\\bootstrap files\\Verizon1_ilec.prn")

hist(data$V1, breaks = 150)

n<-length(data$V1)

qqnorm(data$V1)
qqline(data, col='red')

sim = 1001

means = c(rep(NA,sim))

for (i in 1:sim){
  b_sample <- sample(data$V1, n, replace = TRUE)
  means[i] = mean(b_sample)
}

# Estimated bias from bootstrap
biasboot = mean(means)-mean(data$V1)
biasboot

#t-confidence interval (distribution of T is approx. normal and bias near 0)
conf = 0.95
t=-qt((1-conf)/2,sim-1)
cbind(conf,mean(data$V1)-t*sd(means),mean(data$V1)+t*sd(means))

#percentile confidence intervals
cbind(conf, as.numeric(quantile(means,(1-conf)/2, type=6)),
      as.numeric(quantile(means,(1+conf)/2, type=6))) #follows the smoothed percentile

#Using boot
library(boot)
b.mean=function(x,i) {
  data=x[i]; return( mean(data))
}
out1=boot(data$V1,b.mean,R=1001)
boot.ci(out1, conf = 0.95, type="norm")
boot.ci(out1, conf = 0.95, type="perc")
