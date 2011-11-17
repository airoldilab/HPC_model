# Script to test different generative processes for HPD model

# Generate p-vectors
logit.link <- function(x){1/(1+exp(-x))}
probit.link <- function(x){pnorm(x)}
mean.x <- -15
var.x <- 135
x=rnorm(n=100000,mean=mean.x,sd=sqrt(var.x));y=logit.link(x);hist(y)
length(y[y>0.8])/length(y)
x=rnorm(n=100000,mean=mean.x,sd=sqrt(var.x));y=probit.link(x);hist(y)
length(y[y>0.8])/length(y)
x=rnorm(n=100000,mean=mean.x,sd=sqrt(var.x));hist(as.numeric(x>0))
sum(as.numeric(x>0))/length(y)
