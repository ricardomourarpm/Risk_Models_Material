before=c( 57.7,41.2,52.2,51.5,50.0,42.1,63.7,69.6,58.6, 48.8, 38.3,37.2,53.0,44.1,67.7,48.5,51.2,60.6,25.8,41.4)
after=c(64.2,41.1,50.9,54.4,54.1,44.0,65.5,75.5,57.4, 49.1,41.9,37.7,58.0,50.3,73.3,53.8,55.1,61.2,24.8,40.9)

t.obs=mean(after-before); t.obs

NR=9999

t=rep(NA,NR)

n = length(before)

for(i in 1:NR){
  u=runif(n)
  v1=before*(u<0.5)+after*(u>=0.5)
  v2=after*(u<0.5)+before*(u>=0.5)
  t[i]=mean(v2-v1)
}
hist(t)
pval=mean(t>=t.obs); pval