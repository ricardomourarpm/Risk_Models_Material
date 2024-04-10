x = c(27, 82, 115, 126, 155, 161, 243, 294, 340, 384, 457, 680, 855, 877, 974, 1000, 1000, 1000, 1000, 1000)

n = length(x)

f_exp = function(x,theta){
  dexp(x,1/theta)
}

F_exp = function(x,theta){
  pexp(x,1/theta)
}

minus_like_function1<- function(x,theta){
  -sum(log(f_exp(x[1:15],theta)))-5*log((1-F_exp(1000,theta)))
}

out1 = nlm(minus_like_function1,mean(x),x=x)
out1
MLE = out1$estimate

plot(ecdf(x))
curve(pexp(x,1/out1$estimate),0,1000,add=TRUE)

