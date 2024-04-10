d=c(rep(0,30),0.3,0.7,1.0,1.8,2.1,2.9,2.9,3.2,3.4,3.9)
x=c(0.1,0.5,0.8,0.8,1.8,1.8,2.1,2.5,2.8,2.9,2.9,3.9,4.0,4.0,4.1,4.8,4.8,4.8,
      +     rep(5.0,12),5.0,5.0,4.1,3.1,3.9,5.0,4.8,4.0,5.0,5.0)
v=c(rep(0,3),1,rep(0,5),1,1,0,1,0,0,1,rep(0,16),1,1,rep(0,3),1,0,0)

minusloglikgamma1=function(theta){
    -sum(log((1-v)*(1-pgamma(x,shape=theta[1],scale=theta[2],log=FALSE))+
                   v*dgamma(x,shape=theta[1],scale=theta[2],log=FALSE))-
               log(1-pgamma(d,shape=theta[1],scale=theta[2],log=FALSE)))
}

theta.start=c(3,2)
out=nlm(minusloglikgamma1,theta.start)
out
