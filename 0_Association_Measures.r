#### Correlation coefficients example

x=c(30,47,26,94,67,83,36,77,43,109,56,70)
y=c(4.3,3.6,4.5,2.8,3.3,2.7,4.2,3.9,3.6,2.2,3.1,2.9)

# Pearson's coefficient

pearson_corr_xy=cor(x,y)    # Pearson's coefficient

avg_x=mean(x); sd_x=sd(x)
avg_y=mean(y); sd_y=sd(y)
cbind(avg_x,avg_y,sd_x,sd_y,cor_xy)
    
# just to check formula
cov_xy=cov(x,y)
cov_xy/(sd_x*sd_y) # Pearson's coefficient

# Spearman's coefficient
cor(rank(x),rank(y)) # Spearman's coefficient

cor(x,y,method="spearman")

rank(x); rank(y)
# Alternative computation – Approximate value since we have one tie
d=rank(x)-rank(y); n=12; 1-6*sum(d^2)/(n*(n^2-1)) 

