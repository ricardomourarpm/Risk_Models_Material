# Set the seed for reproducibility (optional)
set.seed(100)

# Generate 10,000 pseudo-uniforms
pseudo_uniforms <- runif(10000)

# Define parameters
alpha <- 3  # Adjust the value of alpha as needed
theta <- 1000  # Adjust the value of theta as needed

# Calculate pseudo-Pareto values
pseudo_pareto_values <- theta * ((1 - pseudo_uniforms) ^ (-1 / alpha) - 1)

Pareto_dist_func=function(x,alpha,theta) 1-(theta/(x+theta))^alpha

# Perform Kolmogorov-Smirnov Test
ks_test <- ks.test(pseudo_pareto_values, Pareto_dist_func, alpha = alpha, theta = theta)
print("Kolmogorov-Smirnov Test:")
print(ks_test)


# Perform Anderson-Darling Test
# install.packages("goftest")
library(goftest)
ad_test <- ad.test(pseudo_pareto_values, Pareto_dist_func, alpha = alpha, theta = theta)
print("\nAnderson-Darling Test:")
print(ad_test)

# Perform Cramer-Von Mises Test
cvm_test <- cvm.test(pseudo_pareto_values, Pareto_dist_func, alpha = alpha, theta = theta)
print("\nCramer-Von Mises Test:")
print(cvm_test)



# Define the number of classes same prob
m <- 100;  aux1=(1:(m-1))/m; aux2=theta*((1-(aux1))^(-1/alpha)-1); # Adjust as needed

lb=c(0,aux2); ub=c(aux2,Inf); counts=rep(NA,m)

for (j in 1:m) 
  counts[j]=sum((pseudo_pareto_values>=lb[j]) & (pseudo_pareto_values<ub[j]))

expected=rep(10000/m,m)

chi2=((counts-expected)^2)/expected

chi2.test=sum(chi2)

p.value=pchisq(chi2.test,m-1,lower.tail=FALSE)
cat("chi-square statistic = ",chi2.test,"\n","p-value = ",p.value,"\n")

result=cbind(lb,ub,counts,expected,chi2)
result

#alternatively
chisq.test(counts, p=expected/10000)


