numbers = c(0,1,2,3)

obs = c(85500,13000,1400,100)

f_p = function(x,lam){dpois(x,lam)}
F_p = function(x,lam){ppois(x,lam)}

minus_l = function(lam) {
  -sum(obs[1:3]*log(f_p(numbers[1:3],lam)))-obs[4]*log(1-F_p(numbers[3],lam))
}

nlm(minus_l,1)

Ej=sum(obs)*c(f_p(numbers[1:3],0.16),1-F_p(numbers[3],0.16))

chi=sum((obs-Ej)^2/Ej)

chi

pvalue = pchisq(chi,4-1-1,lower.tail = FALSE)

pvalue

#reject if pvalue is in fact 0
