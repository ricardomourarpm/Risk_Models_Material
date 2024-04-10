cl=0.95; alpha=12; theta=0.20828469

low.prob=0.025*(1-cl); upp.prob=0.975*(1-cl)
low.x=qgamma(low.prob,shape=alpha,scale=theta)
upp.x=qgamma(upp.prob,shape=alpha,scale=theta)

f=function(x.a){
      p.a=pgamma(x.a,shape=alpha,scale=theta); p.b=cl+p.a
      x.b=qgamma(p.b,shape=alpha,scale=theta)
      return(dgamma(x.a,shape=alpha,scale=theta)-dgamma(x.b,shape=alpha,scale=theta))
      }
f.check=function(x.a){
      p.a=pgamma(x.a,shape=alpha,scale=theta)
      p.b=cl+p.a
      x.b=qgamma(p.b,shape=alpha,scale=theta)
      return(cbind(x.a,x.b,p.a,p.b,dgamma(x.a,shape=alpha,scale=theta),dgamma(x.b,shape=alpha,scale=theta)))
      }


xx=uniroot(f,lower=low.x,upper=upp.x)
xx

f.check(xx$root)
