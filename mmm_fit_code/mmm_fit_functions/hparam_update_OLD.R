# Script to update model hyperparameters and check convergence
#source("check_conv.R")

# First need script to update sigma2 and nu
kappa.nu <- 5
lambda.nu <- 5/kappa.nu
kappa.sigma2 <- 5
lambda.sigma2 <- 0.05/kappa.sigma2

inv.chisq.like <- function(par,tau2.vec){
  nu <- par[1]
  sigma2 <- par[2]
  N.tau2 <- length(tau2.vec)
  no.data.term <- (nu/2)*log(nu/2) - lgamma(nu/2) + (nu/2)*log(sigma2)
  log.like <- N.tau2*no.data.term - (nu/2 + 1)*sum(log(tau2.vec)) -
    (nu*sigma2/2)*sum(1/tau2.vec)
  return(log.like)
}

nu.log.prior <- function(nu,kappa,lambda){
  log.prior <- (kappa-1)*log(nu) - (nu/lambda)
  #log.prior <- -log(nu)
  return(log.prior)
}

sigma2.log.prior <- function(sigma2,kappa,lambda){
  log.prior <- (kappa-1)*log(sigma2) - (sigma2/lambda)
  #log.prior <- -log(sigma2)
  return(log.prior)
}

inv.chisq.post <- function(par,tau2.vec,kappa.nu,lambda.nu,
                           kappa.sigma2,lambda.sigma2){
  # Optimize in log space
  par <- exp(par)
  
  log.post <- inv.chisq.like(par=par,tau2.vec=tau2.vec) +
    nu.log.prior(nu=par[1],kappa=kappa.nu,lambda=lambda.nu) +
      sigma2.log.prior(sigma2=par[2],kappa=kappa.sigma2,lambda=lambda.sigma2)
  return(log.post)
}

joint.inv.chisq.optim <- function(tau2.vec,kappa.nu,lambda.nu,
                           kappa.sigma2,lambda.sigma2){

  # Initialize parameters
  sigma2 <- sigma2.update(tau2.vec=tau2.vec)
  nu <- initialize.nu(tau2.vec=tau2.vec,sigma2=sigma2)

  # Optimize in log space
  par <- log(c(nu,sigma2))

  # Get optimium of joint posterior
  par.out <- optim(par=par,fn=inv.chisq.post,
                   tau2.vec=tau2.vec,
                   kappa.nu=kappa.nu,lambda.nu=lambda.nu,
                   kappa.sigma2=kappa.sigma2,
                   lambda.sigma2=lambda.sigma2,
                   control=list(fnscale=-1))$par

  # Go back to constrained space
  par.out <- exp(par.out)
  names(par.out) <- c("nu","sigma2")
  return(par.out)
}

###############################

nu.profile.log.post <- function(nu,sigma2,tau2.vec,kappa,lambda){
  log.post <- inv.chisq.like(c(nu,sigma2),tau2.vec) +
    nu.log.prior(nu,kappa,lambda)
  return(log.post)
}

sigma2.profile.log.post <- function(nu,sigma2,tau2.vec,kappa,lambda){
  log.post <- inv.chisq.like(c(nu,sigma2),tau2.vec) +
    sigma2.log.prior(sigma2,kappa,lambda)
  return(log.post)
}

nu.optimize <- function(tau2.vec,sigma2,kappa=NULL,lambda=NULL,prior=TRUE){

  if(prior){
    nu.new <- optimize(f=nu.profile.log.post,interval=c(0,1000),maximum=TRUE,
                     tau2.vec=tau2.vec,sigma2=sigma2,kappa=kappa,lambda=lambda)
  } else {
  nu.new <- optimize(f=inv.chisq.like,interval=c(0,1000),maximum=TRUE,
                     tau2.vec=tau2.vec,sigma2=sigma2)}
  return(nu.new)
}

sigma2.optimize <- function(tau2.vec,nu,kappa,lambda){

  # Initialize parameter
  sigma2 <- sigma2.update(tau2.vec=tau2.vec)
  
  sigma2.new <- optimize(f=sigma2.profile.log.post,interval=c(0,1000),maximum=TRUE,
                         tau2.vec=tau2.vec,nu=nu,kappa=kappa.sigma2,lambda=lambda.sigma2)
  return(sigma2.new)
}

# sigma2 update is analytic
sigma2.update <- function(tau2.vec){
  N.tau2 <- length(tau2.vec)
  sigma2.new <- N.tau2/sum(1/tau2.vec)
  return(sigma2.new)
}

# Get derivates of profile likelihood of nu
nu.fd <- function(nu,tau2.vec,sigma2,kappa,lambda){
  N.tau2 <- length(tau2.vec)
  fd <- N.tau2*log(nu/2) - N.tau2*digamma(nu/2) + N.tau2*log(sigma2) -
    sum(log(tau2.vec)) + (kappa-1)/nu - 1/lambda
  return(fd)
}

nu.sd <- function(nu,tau2.vec,sigma2,kappa,lambda){
  N.tau2 <- length(tau2.vec)
  sd <- N.tau2/nu - (N.tau2/2)*trigamma(nu/2) - (kappa-1)/nu^2
  return(sd)
}

# One Newton-Raphson update for nu
nu.update <- function(nu,tau2.vec,sigma2,kappa,lambda,log.param=TRUE){
  
  fd <- nu.fd(nu=nu,tau2.vec=tau2.vec,sigma2=sigma2,
              kappa=kappa,lambda=lambda)
  sd <- nu.sd(nu=nu,tau2.vec=tau2.vec,sigma2=sigma2,
              kappa=kappa,lambda=lambda)
  
  if(log.param){
    # Transform nu to log-nu
    log.nu <- log(nu)
    # Need chain rule
    fd.log <- fd*nu
    sd.log <- nu*(nu*sd + fd)
    nu.log.new <- log.nu - (fd.log/sd.log)
    nu.new <- exp(nu.log.new)
  } else {nu.new <- nu - (fd/sd)}
  return(nu.new)
}

# Function to initialize nu with moment estimator
initialize.nu <- function(tau2.vec,sigma2){
  mean.tau2 <- mean(tau2.vec)
  nu.start <- 2*mean.tau2/(mean.tau2-sigma2)

  # Make sure have reasonable first guess
  #default.value <- sqrt(.Machine$double.eps)
  default.value <- 1
  nu.start <- max(nu.start,default.value)
  return(nu.start)
}

# Newton-Raphson function for nu
nu.newton.raphson <- function(tau2.vec,sigma2,kappa,lambda,
                              reltol=1e-8,iter.max=100,
                              verbose=FALSE){
  
  nu <- initialize.nu(tau2.vec,sigma2)

  nu.vec <- nu

  for(i in 1:iter.max){
    print(nu)
    nu.old <- nu
    nu <- nu.update(nu=nu,tau2.vec=tau2.vec,sigma2=sigma2,
                    kappa=kappa,lambda=lambda)
    nu.vec <- c(nu.vec,nu)
    conv <- check.conv(old.param.vec=nu.old,new.param.vec=nu,
                       reltol=reltol)
    if(conv){break}
  }

  if(verbose){
    if(i < iter.max){print(paste("Newton-Raphson for nu converged in",i,"iterations"))
     } else {print(paste("Newton-Raphson for nu failed to converge in",
                         iter.max,"iterations"))}
    ts.plot(nu.vec)
  }

  #print(nu.vec)

  return(nu)   
}

# Overall function to update hyperparamters
update.hparam <- function(current.param.list,reltol=1e-6,
                          kappa.nu,lambda.nu,kappa.sigma2,
                          lambda.sigma2){

  # Grab relevant parameters out of list
  mu.corpus.vec <- current.param.list$mu.corpus.vec
  tau2.vec <- as.vector(current.param.list$tau2.param.vecs)
  print(summary(tau2.vec))
  psi.old <- current.param.list$psi
  gamma.old <- current.param.list$gamma
  sigma2.old <- current.param.list$sigma2
  nu.old <- current.param.list$nu
  
  # Update hparams
  #psi.new <- mean(mu.corpus.vec)
  #gamma.new <- sd(mu.corpus.vec)
  psi.new <- log(0.05)
  gamma.new <- 0.1

  n.tau2 <- length(tau2.vec)
  nu.new <- mean(log(tau2.vec))
  ## nu.new <- -1
  sigma2.new <- var(log(tau2.vec))*((n.tau2-1)/n.tau2)
  ## sigma2.new <- 2
  ## sigma2.new <- 1

  ## nu.new <- nu.old
  ## sigma2.new <- sigma2.optimize(tau2.vec=tau2.vec,nu=nu.new,kappa=kappa.sigma2,
  ##                               lambda=lambda.sigma2)$maximum
  
  ## par.inv.chisq.new <- joint.inv.chisq.optim(tau2.vec=tau2.vec,
  ##                                            kappa.nu=kappa.nu,
  ##                                            lambda.nu=lambda.nu,
  ##                                            kappa.sigma2=kappa.sigma2,
  ##                                            lambda.sigma2=lambda.sigma2)
  ## nu.new <- par.inv.chisq.new["nu"]
  ## sigma2.new <- par.inv.chisq.new["sigma2"]
  
  ## sigma2.new <- sigma2.update(tau2.vec=tau2.vec)
  ## print(sigma2.new)
  ## sigma2.new <- 0.05
  ## nu.new <- nu.optimize(tau2.vec=tau2.vec,sigma2=sigma2.new,
  ##                       kappa=kappa,lambda=lambda)$maximum
  ## nu.new <- 500
  ## print(nu.new)
  ## nu.new <- nu.newton.raphson(tau2.vec=tau2.vec,sigma2=sigma2.new,
  ##                             kappa=kappa,lambda=lambda)

  # Put new hparams in current.param.list
  current.param.list$psi <- psi.new
  current.param.list$gamma <- gamma.new
  current.param.list$sigma2 <- sigma2.new
  current.param.list$nu <- nu.new
  
  # Check global convergence of hyperparameters
  global.conv <- check.conv(old.param.vec=c(psi.old,gamma.old,sigma2.old,nu.old),
                            new.param.vec=c(psi.new,gamma.new,sigma2.new,nu.new),
                            reltol=1e-6)

  hparam.outlist <- list(current.param.list=current.param.list,
                   global.conv=global.conv)

  return(hparam.outlist)
}
