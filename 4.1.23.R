### EM Algorithm for normal mixture. In this example, for simplicity, we assume that π is known.
data <- Z ### data simulated from two normal mixtures
pi <- 0.3
r <- rep(NA,100) ### responsibilities
Mu1 <- rep(0,11)
### series of estimate of the first expectation
Mu2 <- rep(0,11)
### series of estimate of the second expectation
Mu1[1] <- 0; Mu2[1] <- 1 ### start values
for(j in 1:10)
{
for (i in 1:100)
{
r[i] <- pi*dnorm(Z[i],Mu2[j],1)
/((1-pi)*dnorm(Z[i],Mu1[j],1)
+pi*dnorm(Z[i],Mu2[j],1));
}
Mu1[j+1] <- sum((1-r)*Z)/sum(1-r)
Mu2[j+1] <- sum(r*Z)/sum(r)
}
