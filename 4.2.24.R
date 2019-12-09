### SIMEX
### data=(daty,datx)
### add pseudo-errors with one fixed lamda
### and calculate the naive estimator
simest <- function(n,lamda,B)
{
b1 <- rep(NA,B)
for (j in 1:B)
{
x <- datx+sqrt(lamda)*rnorm(n,0,1)
M <- lm(daty~x)
b1[j] <- coef(M)[2]
}
return(b1)
}
simest(11,0.5,10)
mean(simest(11,0.5,10)) ### stabilizing
### SIM - step
l <- c(0.1,0.2,0.3,0.4,0.5,0.6)
K <- length(l)
b <- rep(NA,K)
B <- 1
n <- length(daty)
for(j in 1:K)
{
b[j] <- mean(simest(n,l[j],B))
}
### EX - step
M1 <- lm(b~l)
### backwards extrapolation, Var=0.5
simb <- coef(M1)[1]-0.5*coef(M1)[2]
