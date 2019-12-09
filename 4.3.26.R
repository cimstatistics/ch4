### Variable selection, Example 4.5.
library(lars)
data(diabetes)
library(leaps) ### subset selection
all <- regsubsets(y~x,data=diabetes)
### stepwise adding variables can be done as follows
### step1
lm0 <- lm(y~1)
A1 <- add1(lm0,~1+x[,1]+x[,2]+x[,3]+x[,4]+x[,5]+
x[,6]+x[,7]+x[,8]+x[,9]+x[,10],test="F")
F1 <- A1$F[2:11] ### partial F statistic for every new added
variable
AIC.1 <- A1$AIC[2:11] ### AIC for every new added variable
A1$AIC[1] ### minimal AIC of the step before
max(F1)
which.max(F1) ### here at bmi= x[,3]
### step2
lm1 <- lm(y~1+x[,3],data=diabetes)
A2 <- add1(lm1,~x[,1]+x[,2]+x[,3]+x[,4]+x[,5]+
x[,6]+x[,7]+x[,8]+x[,9]+x[,10],test="F")
