x_1=c(16,12,13,11,10,9,8,7,5,3,2,0) 
x_2=c(8,10,6,2,8,-1,4,6,-3,-1,-3,0) 
x=cbind(x_1,x_2)
### Using eigenvalues and eigenvectors - centered only
x_1c=(x_1-mean(x_1)); x_2c=(x_2-mean(x_2))
X_c=cbind(x_1c,x_2c)
S=(1/(length(x_1c)-1))*(t(X_c)%*%X_c); S  # covariance matrix

out=eigen(S); out

cbind(out$values[1]/sum(out$values),out$values[2]/sum(out$values))


### Using prcomp function (other solutions are available in R)
out1=prcomp(x,center=T)
out1 # eigenvalues are the squares of st. dev.

summary(out1)

out1$x #scores

out1$rotation # weights / eigenvectors

cor(x,out1$x) # loadings

# using the formula instead of the correlation
z_c=rbind(out1$sdev,out1$sdev)
sd_c=c(sd(x_1),sd(x_2)); sd_c=cbind(sd_c,sd_c)
l=out1$rotation*z_c/sd_c; l  # loadings

