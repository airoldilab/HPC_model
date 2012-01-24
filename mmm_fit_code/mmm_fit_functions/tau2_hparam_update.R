# Script to update model hyperparameters and check convergence
#source("check_conv.R")

loglik <- function(logA, logB, x) sum(dgamma(x,exp(logA),exp(logB),log=TRUE))
loglik <- Vectorize(loglik, c("logA","logB"))

gamma.like <- function(par,tau2.vec){
  #print(par)
  inv.tau2.vec <- 1/tau2.vec
  ## lkappa <- par[1]
  ## llambda <- par[2]
  ## log.like <- loglik(lkappa,llambda,inv.tau2.vec)
  kappa <- exp(par[1])
  lambda <- exp(par[2])
  N.tau2 <- length(tau2.vec)
  no.data.term <- -kappa*log(lambda) - lgamma(kappa)
  log.like <- N.tau2*no.data.term + (kappa-1)*sum(log(inv.tau2.vec)) -
    (1/lambda)*sum(inv.tau2.vec)
  return(log.like)
}

# Functions to get first derivatives in constrained space
get.kappa.fd <- function(lambda,kappa,N.tau2,inv.tau2.vec){
  kappa.fd <- sum(log(inv.tau2.vec)) - N.tau2*log(lambda) -
    N.tau2*digamma(kappa)
  return(kappa.fd)
}

get.lambda.fd <- function(lambda,kappa,N.tau2,inv.tau2.vec){
  lambda.fd <- -N.tau2*kappa/lambda + (1/lambda^2)*sum(inv.tau2.vec)
  return(lambda.fd)
}

# Get gradient of gamma log likelihood in log space
gamma.grad <- function(par,tau2.vec){
  
  kappa <- exp(par[1])
  lambda <- exp(par[2])

  # First derivative wrt kappa
  inv.tau2.vec <- 1/tau2.vec
  N.tau2 <- length(tau2.vec)
  kappa.fd <- get.kappa.fd(lambda,kappa,N.tau2,inv.tau2.vec)

  # First derivative wrt lambda
  lambda.fd <- get.lambda.fd(lambda,kappa,N.tau2,inv.tau2.vec)

  # Get gradient and use chain rule for optimization in log space
  grad <-  c(kappa.fd,lambda.fd)*c(kappa,lambda)

  return(grad)
}


# Function to initialize parameters for gamma
initialize.gamma.param <- function(tau2.vec){
  
  inv.tau2.vec <- 1/tau2.vec
  
  # Initialize lambda
  N.tau2 <- length(tau2.vec)
  lambda.init <- var(inv.tau2.vec)/mean(inv.tau2.vec)

  # Initialize kappa
  mean.tau2 <- mean(tau2.vec)
  kappa.init <- mean(inv.tau2.vec)^2/var(inv.tau2.vec)
  
  # Make sure have reasonable first guess
  default.value <- sqrt(.Machine$double.eps)
  kappa.init <- max(kappa.init,default.value)

  par.init <- c(kappa.init,lambda.init)
  
  return(par.init)
}


joint.optim.gamma <- function(tau2.vec,hessian=FALSE){

  # Initialize parameters
  par.init <- initialize.gamma.param(tau2.vec)
  kappa.init <- par.init[1]
  lambda.opt <- par.init[2]

  # Optimize in log space
  optim.out <- optim(par=log(par.init),fn=gamma.like,
                     gr=gamma.grad,
                     tau2.vec=tau2.vec,
                     method="BFGS",
                     control=list(fnscale=-1),
                     hessian=hessian)

  # Check convergence
  if(!optim.out$convergence==0){
    warning("Optim did not converge")
    print(paste("Optim did not converge with code",optim.out$convergence))
  }

  par.new <- optim.out$par
  kappa.new <- exp(par.new[1])
  lambda.new <- exp(par.new[2])

  out.list <- list(kappa=kappa.new,lambda=lambda.new)
  if(hessian){out.list$hessian <- optim.out$hessian}

  return(out.list)

}

# Function to optimize profile likelihood of gamma
profile.like.kappa <- function(log.kappa,tau2.vec){
  kappa <- exp(log.kappa)
  inv.tau2.vec <- 1/tau2.vec
  lambda.profile <- (1/kappa)*mean(inv.tau2.vec)
  par <- c(log.kappa,log(lambda.profile))
  profile.like <- gamma.like(par,tau2.vec)
  return(profile.like)
}

profile.optim.gamma <- function(tau2.vec){
  optim.out <- optimize(f=profile.like.kappa,interval=c(-10,10),
                        maximum=TRUE,tau2.vec=tau2.vec)
  log.kappa.new <- optim.out$maximum
  kappa.new <- exp(log.kappa.new)
  lambda.new <- (1/kappa.new)*mean(1/tau2.vec)
  out.list <- list(kappa=kappa.new,lambda=lambda.new)
  return(out.list)
}

# Functions to get second derivatives in constrained space
get.lambda.sd <- function(lambda,kappa,N.tau2,inv.tau2.vec){
  lambda.sd <- N.tau2*kappa/(lambda^2) - 2*lambda^(-3)*sum(inv.tau2.vec)
  return(lambda.sd)
}

get.kappa.sd <- function(lambda,kappa,N.tau2,inv.tau2.vec){
  kappa.sd <- -N.tau2*trigamma(kappa)
  return(kappa.sd)
}

get.lambda.kappa.sd <- function(lambda,kappa,N.tau2,inv.tau2.vec){
  lambda.kappa.sd <- -N.tau2/lambda
  return(lambda.kappa.sd)
}

# Function to get Hessian for likelihood in log space
# Need chain rule for all the second derivatives
hessian.gamma <- function(tau2.vec,kappa,lambda){
  
  inv.tau2.vec <- 1/tau2.vec
  N.tau2 <- length(tau2.vec)
  
  lambda.fd <- get.lambda.fd(lambda,kappa,N.tau2,inv.tau2.vec)
  kappa.fd <- get.kappa.fd(lambda,kappa,N.tau2,inv.tau2.vec)
  lambda.sd <- get.lambda.sd(lambda,kappa,N.tau2,inv.tau2.vec)
  kappa.sd <- get.kappa.sd(lambda,kappa,N.tau2,inv.tau2.vec)
  lambda.kappa.sd <- get.lambda.kappa.sd(lambda,kappa,N.tau2,inv.tau2.vec)
  
  h.lambda <- lambda*lambda.fd + lambda^2*lambda.sd
  h.kappa <- kappa*kappa.fd + kappa^2*kappa.sd
  h.lambda.kappa <- kappa*lambda*lambda.kappa.sd
  
  hessian <- matrix(c(h.kappa,h.lambda.kappa,h.lambda.kappa,h.lambda),
                    2,2)
  rownames(hessian) <- colnames(hessian) <- c("kappa","lambda")
  
  return(hessian)

}
