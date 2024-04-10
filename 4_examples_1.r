# Example 13.9 - solution following the book
x=c(27,82,115,126,155,161,243,294,340,384,457,680,855,877,974,1193,1340,1884,2558,15743)
n=length(x);  mu=sum(log(x))/n;  sig2=sum((log(x)-mu)^2)/n; sig=sqrt(sig2)
mu; sig2; sig

I=matrix(c(n/sig2,0,0,2*n/sig2),nrow=2,byrow=TRUE)
I

mat_V=solve(I)
mat_V


# example 13.10 - Following the book
sig3=sig2*sig; sig4=sig2*sig2;
H=matrix(c(-n/sig2,-(2/sig3)*sum(log(x)-mu),-(2/sig3)*sum(log(x)-mu),
             n/sig2-(3/sig4)*sum((log(x)-mu)^2)),nrow=2,byrow=TRUE)
H

matV_H=solve(-H)
matV_H

#using numerical optimization
minuslogliklognorm=function(theta){
    -sum(-log(x)-log(theta[2])-0.5*log(2*pi)-0.5*(( (log(x)-theta[1]) / theta[2] )^2))
    }

# Be aware of the starting point!
# Numerical optimization could be erroneous  (Hessian matrix)
theta.start=c(6,1)
out=nlm(minuslogliklognorm,theta.start,hessian=TRUE)
out

HH = out$hessian

solve(HH)
