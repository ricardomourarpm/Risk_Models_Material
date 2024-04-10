n=c(99,42,29,28,17,9,3)
linf=c(0,7500,17500,32500,67500,125000,300000)
lsup=c(7500,17500,32500,67500,125000,300000,Inf)

loglikgroupedexp=function(theta){
    -sum(n*log(pexp(lsup,rate=1/theta[1])-pexp(linf,rate=1/theta[1]) ) )
    }
theta.start=c(10000)
out=nlm(loglikgroupedexp,theta.start)
out
