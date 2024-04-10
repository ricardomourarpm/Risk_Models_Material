alpha = 3
theta = 1000

true_mean = theta/(alpha-1)
true_var = (alpha*theta^2) / ((alpha-1)^2 * (alpha-2))

Pareto_dist_func=function(x,alpha,theta) 1-(theta/(x+theta))^alpha

true_F_1000 = Pareto_dist_func(1000,alpha, theta)

true_q_0_9 = theta * ((1-0.9)^(-1/alpha)-1)

m_1 = 10000

u = runif(m_1); x_1 = theta * ((1-u)^(-1/alpha)-1)

F1000_sim = mean(x_1<=1000)

n_min = ((1.96/0.01)^2)*(1-F1000_sim)/F1000_sim

F1000_sim; n_min #stop if F1000_sim <= n_min
