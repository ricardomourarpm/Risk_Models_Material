alpha = 1.5
theta = 100

n = 10

NR = 1000

means = rep(NA,NR)

for (i in 1:NR){
  u=runif(n); x = theta * ((1-u)^(-1/alpha) - 1); means[i]=mean(x)
}

mean(means); sd(means)

#install.packages("moments")
library(moments)
cbind(mean(means),median(means),sd(means),skewness(means),kurtosis(means))

qqnorm(means)

ks.test(means, 'pnorm', mean(means), sd(means)/sqrt(n))

plot(ecdf(means), xlim = c(0,2500))
curve(pnorm(x,mean = mean(means), sd = sd(means)/sqrt(n)), from = 0, to = 2500, col="red", add=TRUE)
