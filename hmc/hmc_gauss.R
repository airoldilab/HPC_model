# Load HMC functions
source("hmc_functions.R")


# First try simple spherical normal
cor.mat <- matrix(c(1,0,0,1),2,2)
sigma2 <- 1
Sigma <- sigma2*cor.mat
Sigma.inv <- solve(Sigma)
M.inv <- diag(2)
M.sd <- M.inv
out.hmc <- metro.hmc(ndraws=1000,q.start=c(0,0),nsteps=25,step.size=0.25,
                     U=U.normal,U.grad=U.normal.grad,M.inv=M.inv,
                     M.sd=M.sd,M.diag=FALSE,Sigma.inv=Sigma.inv,debug=TRUE)
plot(out.hmc)
plot(out.hmc[,1])


# Try simple spherical normal with different varainces
cor.mat <- matrix(c(1,0,0,1),2,2)
sigma2 <- c(1,10)
Sigma <- sigma2*cor.mat
Sigma.inv <- solve(Sigma)
#M.inv <- Sigma.inv
#M.sd <- t(chol(Sigma))
M.inv <- Sigma
M.sd <- t(chol(Sigma.inv))
out.hmc <- metro.hmc(ndraws=1000,q.start=c(0,0),nsteps=25,step.size=0.1,
                     U=U.normal,U.grad=U.normal.grad,M.inv=M.inv,
                     M.sd=M.sd,M.diag=FALSE,Sigma.inv=Sigma.inv,debug=FALSE)
plot(out.hmc)
par(mfrow=c(1,2))
plot(out.hmc[,1])
plot(out.hmc[,2])
par(mfrow=c(1,2))
plot(out.hmc[,1],type="l")
plot(out.hmc[,2],type="l")
par(mfrow=c(1,2))
hist(out.hmc[,1],"fd")
hist(out.hmc[,2],"fd")

# What if only used diagonal mass matrix?
M.inv <- diag(2)
M.sd <- M.inv
out.hmc <- metro.hmc(ndraws=1000,q.start=c(0,0),nsteps=25,step.size=0.075,
                     U=U.normal,U.grad=U.normal.grad,M.inv=M.inv,
                     M.sd=M.sd,M.diag=FALSE,Sigma.inv=Sigma.inv,debug=FALSE)
plot(out.hmc)
par(mfrow=c(1,2))
plot(out.hmc[,1])
plot(out.hmc[,2])
plot(out.hmc[,1],type="l")
plot(out.hmc[,2],type="l")


# Want to sample from highly correlated normal

# First try diagonal mass matrix
cor.mat <- matrix(c(1,0.9,0.9,1),2,2)
sigma2 <- c(1,10)
Sigma <- sigma2*cor.mat
Sigma.inv <- solve(Sigma)
M.inv <- diag(2)
M.sd <- M.inv
out.hmc <- metro.hmc(ndraws=1000,q.start=c(0,0),nsteps=25,step.size=0.25,
                     U=U.normal,U.grad=U.normal.grad,M.inv=M.inv,
                     M.sd=M.sd,M.diag=FALSE,Sigma.inv=Sigma.inv,debug=FALSE)
plot(out.hmc)
par(mfrow=c(1,2))
plot(out.hmc[,1])
plot(out.hmc[,2])
par(mfrow=c(1,2))
plot(out.hmc[,1],type="l")
plot(out.hmc[,2],type="l")
par(mfrow=c(1,2))
hist(out.hmc[,1],"fd")
hist(out.hmc[,2],"fd")


# Next try a mass matrix with the correct curvature
#M.inv <- Sigma.inv
#M.sd <- t(chol(Sigma))
M.inv <- Sigma
M.sd <- t(chol(Sigma.inv))
out.hmc <- metro.hmc(ndraws=1000,q.start=c(0,0),nsteps=25,step.size=0.1,
                     U=U.normal,U.grad=U.normal.grad,M.inv=M.inv,
                     M.sd=M.sd,M.diag=FALSE,Sigma.inv=Sigma.inv,debug=FALSE)

plot(out.hmc)
par(mfrow=c(1,2))
plot(out.hmc[,1])
plot(out.hmc[,2])
par(mfrow=c(1,2))
plot(out.hmc[,1],type="l")
plot(out.hmc[,2],type="l")
par(mfrow=c(1,2))
hist(out.hmc[,1],"fd")
hist(out.hmc[,2],"fd")


# Try Langevin diffusion for this problem
M.inv <- Sigma.inv
M.sd <- t(chol(Sigma))
out.hmc <- metro.hmc(ndraws=10000,q.start=c(0,0),nsteps=1,step.size=0.15,
                     U=U.normal,U.grad=U.normal.grad,M.inv=M.inv,
                     M.sd=M.sd,M.diag=FALSE,Sigma.inv=Sigma.inv,debug=FALSE)

plot(out.hmc)
par(mfrow=c(1,2))
plot(out.hmc[,1])
plot(out.hmc[,2])
par(mfrow=c(1,2))
plot(out.hmc[,1],type="l")
plot(out.hmc[,2],type="l")
par(mfrow=c(1,2))
hist(out.hmc[,1],"fd")
hist(out.hmc[,2],"fd")


# Check Chol for inverse matrix
M.inv.check <- Sigma.inv
M.inv.chol.check <- chol(M.inv.check)
M.sd.check <- backsolve(M.inv.chol.check,diag(nrow(M.inv.check)))
forwardsolve(t(M.inv.chol.check),diag(nrow(M.inv.check)))

M <- Sigma.inv
M.sd <- t(chol(M))
M.inv.chol <- forwardsolve(M.sd,diag(nrow(M)))
M.inv <- t(M.inv.chol)%*%M.inv.chol
