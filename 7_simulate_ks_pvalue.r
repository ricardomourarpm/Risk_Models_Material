x = c(11.79,11.25,6.83,10.47,13.60,10.60,17.40,10.99,16.45,12.47,8.19,13.46,
      13.82,11.93,7.47,10.09,14.26, 12.04,12.13,9.66)

a = ks.test(x, 'pnorm', 10, 3)
D = a$statistic
a

NR = 10000; n = length(x)

means = rep(NA,NR)

for (i in 1:NR){
  y = rnorm(20,10,3); a = ks.test(y,'pnorm', 10, 3)
  means[i] = a$statistic
}

p_value = mean(means>D); p_value