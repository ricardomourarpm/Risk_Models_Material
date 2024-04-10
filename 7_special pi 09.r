alpha = 3
theta = 1000

true_mean = theta/(alpha-1)
true_var = (alpha*theta^2) / ((alpha-1)^2 * (alpha-2))

Pareto_dist_func=function(x,alpha,theta) 1-(theta/(x+theta))^alpha

true_F_1000 = Pareto_dist_func(1000,alpha, theta)

true_pi_0_9 = theta * ((1-0.9)^(-1/alpha)-1)

set.seed(111)
m_1 = 146000

u = runif(m_1); x_1 = theta * ((1-u)^(-1/alpha)-1)

pi_0.9 = quantile(x_1,0.9)

a = floor(.9*m_1-0.5-1.96*sqrt(0.9*0.1*m_1))
b = ceiling(.9*m_1+0.5+1.96*sqrt(0.9*0.1*m_1))

pi_0.9; 0.01*pi_0.9;

pi_0.9-sort(x_1)[a] <= 0.01*pi_0.9; sort(x_1)[a]
sort(x_1)[b]-pi_0.9 <= 0.01*pi_0.9; sort(x_1)[b]
