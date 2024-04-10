x=c(82,115,126,155,161,243,294,340,384,457,680,855,877,974,1193,1340,1884,2558,3476)
theta_hat=mean(x)-50
trunc_expon_dist=function(x,theta,t) {
  # x must be greater than or equal to t
  (exp(-t/theta)-exp(-x/theta))/exp(-t/theta)
  }
ks.test(x,"trunc_expon_dist",theta=theta_hat,t=50)


y=trunc_expon_dist(x,theta_hat,50)
n=length(x); Fn=1:n/n; Fn_minus=Fn-1/n
D=max(abs(y-Fn),abs(y-Fn_minus))
D
