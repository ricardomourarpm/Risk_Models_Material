Treatment=c(24,43,58,71,43,49,61,44,67,49,53,56,59,52,62,54,57,33,46,43,57)
Control=c(42,43,55,26,62,37,33,41,19,54,20,85,46,10,17,60,53,42,37,42,55,28,48)

T.obs=mean(Treatment)-mean(Control); T.obs

# permutation test
x=c(Treatment,Control)
n1=length(Treatment); n=length(x)

NR=9999
res=rep(NA,NR)

for(i in 1:NR) {
  xx=sample(x,n,replace=F);
  res[i]=mean(xx[1:n1])-mean(xx[(n1+1):n])
}

hist(res)
pval=mean(res>=T.obs)
pval

factorial(n)/(factorial(n_1)*factorial(n-n_1)) # problematic
