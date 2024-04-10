fn1=function(x){
  # x[1]=alpha     x[2]=theta
  eq1=0.7-(x[2]/(185.6+x[2]))^x[1]
  eq2=0.2-(x[2]/(1310.6+x[2]))^x[1]
  return(c(eq1,eq2))
}

fn2=function(x){
  # x[1]=alpha     x[2]=theta
  eq1=log(0.7)-x[1]*log(x[2]/(185.6+x[2]))
  eq2=log(0.2)-x[1]*log(x[2]/(1310.6+x[2]))
  return(c(eq1,eq2))
}
fn3=function(x){
  # x=theta
  eq1=(log(0.2)/log(0.7))-(log(x)-log(1310.6+x))/(log(x)-log(185.6+x))
  return(eq1)
}

#install.packages("nleqslv")

require(nleqslv)
nleqslv(1000,fn3)  # using fn3
nleqslv(c(1,1),fn1)
nleqslv(c(1,1),fn2)


fn1_sq=function(x) return(crossprod(fn1(x),fn1(x)))
fn2_sq=function(x) return(crossprod(fn2(x),fn2(x)))
fn3_sq=function(x) return(fn3(x)^2)

nlm(fn1_sq,c(1,5))
nlm(fn2_sq,c(1,5))
nlm(fn3_sq,0.1)
