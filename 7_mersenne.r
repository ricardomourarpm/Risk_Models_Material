a = 123
c = 1000
n_0 = 135

n_t = c(n_0,array(NA,25))

for (i in 1:25){
  n_t[i+1] = (a*n_t[i])%%c
}

n_t/c