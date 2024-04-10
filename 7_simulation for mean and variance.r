alpha = 3
theta = 1000

true_mean = theta/(alpha-1)
true_var = (alpha*theta^2) / ((alpha-1)^2 * (alpha-2))

Pareto_dist_func=function(x,alpha,theta) 1-(theta/(x+theta))^alpha

true_F_1000 = Pareto_dist_func(1000,alpha, theta)

true_q_0_9 = theta * ((1-0.9)^(-1/alpha)-1)

m_1 = 10000

u = runif(m_1); x_1 = theta * ((1-u)^(-1/alpha)-1)

var_sim = var(x_1)
mean_sim = mean(x_1)
n_min = ((1.96/0.01)^2)*(var_sim/(mean_sim^2))

mean_sim; var_sim; n_min

m_2 = floor(n_min)+1

u = runif(m_2-m_1); x_2 = theta * ((1-u)^(-1/alpha)-1)

x = c(x_1,x_2)
mean_estimate = mean(x)
var_estimate = var(x)
n_min = ((1.96/0.01)^2)*(var_estimate/(mean_estimate^2))

mean_estimate; var_estimate; n_min; m_2 # if n_min greater that m_2, repeat

