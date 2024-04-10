mu = 10
std = 3

n = 5

NR = 1000

means = rep(NA,NR)

for (i in 1:NR){
  x = rnorm(n, mu, std); means[i] = mean(x)
}

mean(means); sd(means)

ks.test(means, 'pnorm', mu, std/sqrt(n))

breaks=seq(4,16,0.5)
points=c(seq(5+0.5/2,16-0.5/2,0.5),10); points=sort(points)
dens=dnorm(points,mu,std/sqrt(n))
hist(means,breaks,prob=TRUE)
lines(points,dens,type="l")
