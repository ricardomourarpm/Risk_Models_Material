dta=read.table("C:\\Users\\ricar\\OneDrive - FCT NOVA\\ISEG\\Risk Models\\bootstrap files\\baseball.prn",header=T)
attach(dta) #Salary and Average
T.obs=cor(Salary,Average)^2
T.obs
# permutation test
n=length(Salary)
NR=9999
res=rep(NA,NR)
for(i in 1:NR) {
  xx=sample(Salary,n,replace=F);
  res[i]=cor(xx,Average)^2
}
hist(res)
pval=mean(res>=T.obs); pval
pval