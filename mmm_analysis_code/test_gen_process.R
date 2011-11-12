# Script to test different generative processes for HPD model

# Generate p-vectors
logit.link <- function(x){1/(1+exp(-x))}
probit.link <- function(x){pnorm(x)}
x=rnorm(n=100000,mean=-70,sd=40);y=logit.link(x);hist(y)
length(y[y>0.8])/length(y)
x=rnorm(n=100,mean=-70,sd=40);hist(probit.link(x))
x=rnorm(n=100,mean=-2,sd=1);hist(as.numeric(x>0))
