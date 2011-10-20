# Script to update model hyperparameters and check convergence
#source("check_conv.R")

inv.chisq.like <- function(par,tau2.vec){
  nu <- exp(par[1])
  sigma2 <- exp(par[2])
  N.tau2 <- length(tau2.vec)
  no.data.term <- (nu/2)*log(nu/2) - lgamma(nu/2) + (nu/2)*log(sigma2)
  log.like <- N.tau2*no.data.term - (nu/2 + 1)*sum(log(tau2.vec)) -
    (nu*sigma2/2)*sum(1/tau2.vec)
  return(log.like)
}

# Get gradient of inv.chisq log likelihood
inv.chisq.grad <- function(par,tau2.vec){
  
  nu <- exp(par[1])
  sigma2 <- exp(par[2])

  # First derivative wrt nu
  N.tau2 <- length(tau2.vec)
  nu.fd2 <- N.tau2*log(0.5*nu) + N.tau2 - N.tau2*digamma(nu/2) + N.tau2*log(sigma2) -
    sum(log(tau2.vec)) - sigma2*sum(1/tau2.vec)
  nu.fd <- 0.5*nu.fd2

  # First derivative wrt sigma2
  sigma2.fd2 <- N.tau2*nu/sigma2 - nu*sum(1/tau2.vec)
  sigma2.fd <- 0.5*sigma2.fd2

  # Get gradient and use chain rule for optimization in log space
  grad <-  c(nu.fd,sigma2.fd)*c(nu,sigma2)

  return(grad)
}


# Function to initialize parameters for inv.chisq
initialize.invchisq.param <- function(tau2.vec){

  # Initialize sigma2
  N.tau2 <- length(tau2.vec)
  sigma2.init <- N.tau2/sum(1/tau2.vec)

  # Initialize nu
  mean.tau2 <- mean(tau2.vec)
  nu.init <- 2*mean.tau2/(mean.tau2-sigma2.init)

  # Make sure have reasonable first guess
  default.value <- sqrt(.Machine$double.eps)
  nu.init <- max(nu.init,default.value)

  par.init <- c(nu.init,sigma2.init)
  
  return(par.init)
}


optim.inv.chisq <- function(tau2.vec){

  # Initialize parameters
  par.init <- initialize.invchisq.param(tau2.vec)
  print(par.init)

  # Optimize in log space
  optim.out <- optim(par=log(par.init),fn=inv.chisq.like,
                     gr=inv.chisq.grad,
                     tau2.vec=tau2.vec,
                     method="BFGS",
                     control=list(fnscale=-1))

  # Check convergence
  if(!optim.out$convergence==0){
    warning("Optim did not converge")
    print(paste("Optim did not converge with code",optim.out$convergence))
  }

  par.new <- optim.out$par
  nu.new <- exp(par.new[1])
  sigma2.new <- exp(par.new[2])

  return(list(nu=nu.new,sigma2=sigma2.new))

}
