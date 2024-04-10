x=c(27,82,115,126,155,161,243,294,340,384,457,680,855,877,974,1193,1340,1884,2558,15743)

minusloglikgamma=function(theta){
    -sum(dgamma(x,shape=theta[1],scale=theta[2],log=TRUE))
    }

loglikgamma=function(a,b){
      sum(dgamma(x,shape=a,scale=b,log=TRUE))
      }

theta.start=c(mean(x)*mean(x)/var(x),var(x)/mean(x))
out=nlm(minusloglikgamma,theta.start,hessian=TRUE)
out

# Independent confidence intervals
theta_mv=out$estimate
invH=solve(-out$hessian) # The function is minus the loglikelihood
theta_mv_var=-diag(invH)
linf=theta_mv-1.96*sqrt(theta_mv_var); lsup=theta_mv+1.96*sqrt(theta_mv_var)
linf; lsup;


# Confidence region
q=qchisq(0.05,2,lower.tail=FALSE)
cc=-out$minimum-0.5*q   # The function is minus the loglikelihood

a=seq(.5*linf[1],2*lsup[1],(2*lsup[1]-.5*linf[1])/81)
b=seq(.5*linf[2],2*lsup[2],(2*lsup[2]-.5*linf[2])/100)

z=array(0,dim=c(length(a),length(b)))
for(i in 1:length(a)) {
  for(j in 1:length(b)) {
    z[i,j]=loglikgamma(a[i],b[j])
    }
  }
persp(a,b,z,theta=30,phi=30,ticktype="detailed")
contour(a,b,z,level=c(cc))

