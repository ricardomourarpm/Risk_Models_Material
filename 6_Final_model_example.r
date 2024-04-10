x = c(82, 115, 126, 155, 161, 243, 294, 340, 384, 457, 680, 855, 877, 974, 1193, 1340, 1884, 2558, 3476)

n = length(x)

# define functions Exp and weibull

f_exp = function(x,theta){
  dexp(x,1/theta)/(1-pexp(50,1/theta))
}

F_exp = function(x,theta){
  (pexp(x,1/theta)-pexp(50,1/theta))/(1-pexp(50,1/theta))
}

f_weib = function(x,tau,theta){
  dweibull(x,shape = tau, scale = theta)/(1-pweibull(50,shape = tau, scale = theta))
}

F_weib = function(x,tau,theta){
  (pweibull(x, shape = tau, scale = theta)-pweibull(50, shape = tau, scale = theta))/(1-pweibull(50, shape = tau, scale = theta))
}



###############################################
# estimate parameters

minus_like_function1<- function(x,theta){
  -sum(log(f_exp(x,theta)))
}

out1 = nlm(minus_like_function1,700,x=x)

theta_MLE = out1$estimate # we already knew it was mean(x)-50

theta_MLE-mean(x)+50 # observe

minus_like_function2<- function(x,param){
  tau=param[1]; theta=param[2]
  -sum(log(f_weib(x,tau,theta)))
}

mean(x)

out2 = nlm(minus_like_function2,c(1,mean(x)),x=x)
out2

tau_MLE = out2$estimate[1]
theta_MLE2 = out2$estimate[2]


########################################################
# Present results
#KS
ks.test(x, F_exp, theta = theta_MLE)

ks.test(x, F_weib, tau = tau_MLE, theta = theta_MLE2)

#AD
library(goftest)

ad.test(x, F_exp, theta = theta_MLE)

ad.test(x, F_weib, tau = tau_MLE, theta = theta_MLE2)

#chi gof
c_minus = c(50, 150, 250, 500, 1000, 2000)

c_plus = c(c_minus[-1], Inf) # Inf is used for the last bin

Oj = c(3,3,4,4,3,2)

Ej = n*(F_exp(c_plus,theta = theta_MLE)-F_exp(c_minus,theta = theta_MLE))

chi_stat1 = sum((Oj-Ej)^2/Ej)
chi_stat1

pvalue1 = 1-pchisq(chi_stat1,6-1-1)
pvalue1

Ej = n*(F_weib(c_plus,tau = tau_MLE, theta = theta_MLE2)-F_weib(c_minus,tau = tau_MLE, theta = theta_MLE2))

chi_stat2 = sum((Oj-Ej)^2/Ej)
pvalue2 = 1-pchisq(chi_stat2,6-1-2)
pvalue2


#SBC
-out1$minimum
-out2$minimum

-out1$minimum-1/2*log(n)
-out2$minimum-2/2*log(n)

# test H0:exp or H1:weibull
T=-2*(-out1$minimum+out2$minimum)
T
pvalue = 1-pchisq(T,1)
pvalue
