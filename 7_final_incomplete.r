M = 10000

oversized_n = 50

res = array(NA,M)

n_claims = array(NA, M)

for (k in 1:M){
  u = runif(oversized_n); v = -0.2*log(1-u) # generate times between events
  c = cumsum(v) # cumulative sum of all times
  n = sum(c<1) # count the times before 1 year
  c=c[1:n] # select those
  n_claims[i] = n
  if (n==0) {res[i]=0} else {
    u=runif(n); x = 1000 * ((1-u)^(-1/3)-1) # generate claim amounts pareto 3, 1000
    u=runif(n); l=pmax((1/6)*log(x)*((-log(1-u))^(2/3)),0) # L_j
    t=c+l
    pay = x*exp(-0.06*t) # discounted values for each claim
    res[i]=sum(pay)
  }
  
}