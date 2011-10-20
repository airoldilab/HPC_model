# Script to set up importance sampler for tau2 hyperparameters

library("mvtnorm")

# Function to convert hparams from gamma scale to
# inv.chisq scale (and back)
convert.hparams <- function(par,to.invchisq=TRUE){
  if(to.invchisq){
    kappa <- par[1]
    lambda <- par[2]
    nu <- 2*kappa
    sigma2 <- 1/(kappa*lambda)
    par.convert <- c(nu,sigma2)
  } else {
    nu <- par[1]
    sigma2 <- par[2]
    kappa <- nu/2
    lambda <- 2/(nu*sigma2)
    par.convert <- c(kappa,lambda)
  }
  return(par.convert)
}

# Proposal density
gamma.prop.dens <- function(gamma.vec,prop.mean,prop.inv.cov){
  deviat.vec <- gamma.vec-prop.mean
  log.prop.dens <- -0.5*deviat.vec%*%prop.inv.cov%*%deviat.vec
  return(as.numeric(log.prop.dens))
}


# Function to generate proposal draws
gamma.prop.draw <- function(ndraws,prop.mean,prop.cov){
  gamma.vecs <- rmvnorm(n=ndraws,mean=prop.mean,sigma=prop.cov)
  return(gamma.vecs)
}


# Function to evaluate importance weights of draws
# Takes N x (K+1) matrix of draws as input
gamma.import.weights <- function(gamma.draws,prop.mean,prop.inv.cov,
                                 tau2.vec){
  
  # Get joint density of each draw in target and proposal dists
  target.log.dens <- apply(gamma.draws,1,gamma.like,
                           tau2.vec=tau2.vec)
  prop.log.dens <-  apply(gamma.draws,1,gamma.prop.dens,
                          prop.mean=prop.mean,
                          prop.inv.cov=prop.inv.cov)
  log.weights <- target.log.dens-prop.log.dens
  log.weights.center <- log.weights-mean(log.weights)
  #weights <- exp(log.weights.center)
  return(log.weights.center)
}


# Importance sampler
gamma.import.sampler <- function(current.param.list,
                                 ndraws=10000,prop.scale=1,
                                 par.start=NULL){

  # Get vector of discrimination parameters
  tau2.vec <- as.vector(current.param.list$tau2.param.vecs)
  
  # Get normal approx to posterior
  optim.out <- profile.optim.gamma(tau2.vec)
  ## optim.out <- joint.optim.gamma(tau2.vec,hessian=FALSE)
  
  kappa.fit <- optim.out$kappa
  lambda.fit <- optim.out$lambda
  prop.mean <- log(c(kappa.fit,lambda.fit))
  prop.inv.cov <- -hessian.gamma(tau2.vec=tau2.vec,lambda=lambda.fit,
                                 kappa=kappa.fit)
  prop.cov <- solve(prop.inv.cov)
  
  # Get draws from proposal dist centered on posterior mode
  gamma.draws <- gamma.prop.draw(ndraws=ndraws,prop.mean=prop.mean,
                                 prop.cov=prop.cov)
  if(!is.null(par.start[1])){gamma.draws <- rbind(log(par.start),gamma.draws)
                           rownames(gamma.draws) <- NULL}
  
  # Get important weights for samples
  log.weights <- gamma.import.weights(gamma.draws=gamma.draws,
                                      prop.mean=prop.mean,
                                      prop.inv.cov=prop.inv.cov,
                                      tau2.vec=tau2.vec)

  # Transform draws into inv.chisq scale
  exp.gamma.draws <- exp(gamma.draws)
  inv.chisq.draws <- t(apply(exp.gamma.draws,1,convert.hparams))
  
  ## # Get expectation of sufficient stats
  ## expect.mean <- colMeans(inv.chisq.draws)
  ## expect.covar <- var(inv.chisq.draws)
  
  return(list(draws=inv.chisq.draws,log.weights=log.weights))
              ## expect.mean=expect.mean,expect.covar=expect.covar,
              ## weights=weights))
}



#################
# HMC functions #
#################

U.gamma <- function(...){-1*gamma.like(...)}
U.grad.gamma <- function(...){-1*gamma.grad(...)}


# Function to get HMC samples from gamma posterior
hmc.gamma <- function(ndraws,step.size,nsteps,current.param.list,
                      debug=FALSE,last.draw=FALSE){

  # Extract tau2 hyperparameters
  nu.old <- current.param.list$nu
  sigma2.old <- current.param.list$sigma2
  tau2.vec <- as.vector(current.param.list$tau2.param.vecs)
  # Get old draws on gamma scale
  par.old <- convert.hparams(par=c(nu.old,sigma2.old),to.invchisq=FALSE)
  ## kappa <- par.old[1]
  ## lambda <- par.old[2]
  log.par.old <- log(par.old)
  
  # Evaluate hessian at conditional posterior mode
  optim.out <- profile.optim.gamma(tau2.vec)
  kappa.fit <- optim.out$kappa
  lambda.fit <- optim.out$lambda
  hes <- hessian.gamma(tau2.vec,kappa.fit,lambda.fit)
  
  # Get mass matrix from hessian
  hes2mass.out <- hes2mass(hes=hes,hes.diag=FALSE)
  M.inv <- hes2mass.out$M.inv
  M.sd <- hes2mass.out$M.sd
  M.diag <- hes2mass.out$M.diag
  
  # Run HMC
  out.hmc <- metro.hmc(ndraws=ndraws,q.start=log.par.old,nsteps=nsteps,
                       step.size=step.size,U=U.gamma,U.grad=U.grad.gamma,
                       M.sd=M.sd,M.inv=M.inv,M.diag=M.diag,
                       debug=debug,tau2.vec=tau2.vec)
  
  if(ndraws == 1){out.hmc <- matrix(out.hmc,nrow=1,byrow=TRUE)}
  
  # Transform draws into inv.chisq scale
  exp.gamma.draws <- exp(out.hmc)
  inv.chisq.draws <- t(apply(exp.gamma.draws,1,convert.hparams))
  out.hmc <- inv.chisq.draws
  
  # Only return last draw if requested
  if(last.draw){out.hmc <- out.hmc[nrow(out.hmc),]}
  
  return(out.hmc)
}
