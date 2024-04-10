x = read.csv("C:\\Users\\ricar\\OneDrive - FCT NOVA\\ISEG\\Risk Models\\Scripts\\Data_B.csv")

mean(x$payment)

x = c(x$payment)
# 1ST SOLUTION: USE FUNCTION nlm
# As nlm minimizes a function we introduce minus the log-lik
minusloglikgamma=function(param,x){
  alpha=param[1]; theta=param[2]
  -sum(dgamma(x,shape=alpha,scale=theta,log=TRUE))
  }

param.start=c(1,1000) # starting values – important point
out1=nlm(minusloglikgamma,param.start,x=x)
out1

# 2ND SOLUTION: USE FUNCTION maxLik, LIBRARY maxLik
# As maxLik maximizes a function we introduce the log-lik
loglikgamma=function(param,x){
  alpha=param[1]; theta=param[2]
  sum(dgamma(x,shape=alpha,scale=theta,log=TRUE))
  }
# param.start has already been defined
#install.packages("maxLik")
library(maxLik)
out2=maxLik(loglikgamma,start=param.start,x=x)
out2
