# Script to make sure getting right fit for inverse-chi sq parameters

library(geoR)
source("hparam_update.R")
source("tau2_hparam_update.R")
source("check_conv.R")

sigma2 <- 1
nu <- 10
kappa <- nu/2
lambda <- 2/(nu*sigma2)
n.gen <- 10000
c(kappa,lambda)

#curve(dinvchisq(x,df=nu,scale=sigma2),0,5)
#curve(dgamma(1/x,shape=kappa,scale=lambda)*(1/x^2),add=TRUE)

tau2.vec <- rinvchisq(n=n.gen,df=nu,scale=sigma2)
inv.tau2.vec <- 1/tau2.vec
#inv.tau2.vec <- rgamma(n.gen,shape=10,scale=5)
var(inv.tau2.vec)/mean(inv.tau2.vec)
mean(inv.tau2.vec)^2/var(inv.tau2.vec)
optim.out <- joint.optim.gamma(tau2.vec,hessian=TRUE)
print(optim.out)
kappa.fit <- optim.out$kappa
lambda.fit <- optim.out$lambda
hessian.gamma(tau2.vec,kappa.fit,lambda.fit)

par.new <- optim.inv.chisq(tau2.vec)

npoints <- 100
eps <- 0.1
x <- seq(log(kappa.fit)-eps,log(kappa.fit)+eps,length.out=npoints)
y <- seq(log(lambda)-eps,log(lambda)+eps,length.out=npoints)
grid <- expand.grid(x=x,y=y)
z.vec <- apply(grid,1,gamma.like,tau2.vec=tau2.vec)
z.mat <- matrix(z.vec,npoints,npoints)
contour(x=x,y=y,z=exp(z.mat-max(z.mat)))
points(log(kappa),log(lambda))


library(lattice)
data.mat <- data.frame(x=grid[,1],y=grid[,2],z=exp(z.vec-max(z.vec)))
wireframe(z~x*y,data=data.mat,drape=TRUE,scale=list(arrows=FALSE))

optim(par=c(0,0),fn=inv.chisq.like,tau2.vec=tau2.vec)


sigma2.new <- sigma2.optimize(tau2.vec=tau2.vec,nu=nu,
                              kappa=kappa.sigma2,
                              lambda=lambda.sigma2)$maximum
sigma2.new

par.inv.chisq.new <- joint.inv.chisq.optim(tau2.vec=tau2.vec[1:10],
                                           kappa.nu=kappa.nu,
                                           lambda.nu=lambda.nu,
                                           kappa.sigma2=kappa.sigma2,
                                           lambda.sigma2=lambda.sigma2)
print(par.inv.chisq.new)





############################################################
sigma2.fit <- sigma2.update(tau2.vec)
nu.fit <- nu.newton.raphson(tau2.vec=tau2.vec,sigma2=sigma2.fit,
                            kappa=kappa,lambda=lambda,
                            verbose=TRUE)

curve(nu.profile.like(nu=x,tau2.vec=tau2.vec,sigma2=sigma2.fit),
      from=0,to=10)
curve(nu.profile.log.post(nu=x,tau2.vec=tau2.vec,sigma2=sigma2.fit,
                          kappa=kappa,lambda=lambda),
      from=0.01,to=10)
curve(nu.sd(nu=x,tau2.vec=tau2.vec,sigma2=sigma2.fit,
            kappa=kappa,lambda=lambda),from=0.01,to=10)
nu.fit <- nu.optimize(tau2.vec=tau2.vec,sigma2=sigma2.fit,
                      kappa=kappa,lambda=lambda,prior=TRUE)
c(sigma2.fit,nu.fit)


