theta_hat=2561.14; 
alpha_hat=0.55616; 
n=20; 
B=1000; 

res=rep(NA,B)
for(i in 1:B){
  x=rgamma(n,shape=alpha_hat,scale=theta_hat)
  res[i]=mean(x)
}

ks.test(res,"pgamma",shape=n*alpha_hat,scale=theta_hat/n)

hist(res,prob=TRUE)

x=seq(min(res),max(res),length=100)
dens=dgamma(x,shape=11.1232,scale=128.057)
lines(x,dens,type="l")

qqplot(res,rgamma(B,shape=11.1232,scale=128.057),main="Gamma Q-Q Plot")
