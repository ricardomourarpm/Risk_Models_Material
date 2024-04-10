x=seq(0,6,by=0.2)
plot(x,dgamma(x,shape=1,scale=1),type="l",ylab="density",xlab="alpha")
y=dgamma(x,shape=12,scale=0.208285) # posterior
lines(x,y,type="l",lty=2)
text(3.5,0.45,"posterior"); text(0.8,0.7,"prior")

y=seq(100,400,by=20)
yy1=2*(y^(-1))*((1+log(y/100))^(-3));
plot(y,yy1,type="l",ylab="predictive density",xlab="y")
yy2=3.757995*factorial(12)*(y^(-1))*((0.195951+log(y))^(-13));
lines(y,yy2,type="l",lty=2)
text(130,0.005,"before"); text(150,0.010,"after")
