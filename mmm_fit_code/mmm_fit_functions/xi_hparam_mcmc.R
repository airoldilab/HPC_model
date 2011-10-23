# Script to set up conjugate conditional sampler for xi hyperparameters

library("geoR")
library("MCMCpack")
library("mvtnorm")

xi.hparam.draw <- function(xi.param.vecs,D,K,full.Sigma=FALSE,
                           lambda2.old=NULL,Sigma.old=NULL,
                           kappa=NULL,Sigma.0=NULL){

  # Get shared quantities of interest
  eta.post.mean <- colMeans(xi.param.vecs)
  
  # Two types of updates: one for full Sigma matrix, one for scalar
  # times the identity
  if(full.Sigma){
    # Draw new eta.vec given Sigma
    eta.post.var <- Sigma.old/D
    eta.vec.new <- as.vector(rmvnorm(n=1,mean=eta.post.mean,
                                     sigma=eta.post.var))
    names(eta.vec.new) <- colnames(xi.param.vecs)

    # Draw new Sigma matrix given new eta
    xi.demean.mat <- sweep(xi.param.vecs, 2, eta.vec.new, "-")
    W <- crossprod(xi.demean.mat)
    ## print(paste("dim W:",dim(W)))
    ## print(paste("dim Sigma.0:",dim(Sigma.0)))
    Sigma.df <- kappa + D
    Sigma.new <- riwish(v=Sigma.df, S=kappa*Sigma.0 + W)
    
  } else {
    # Draw new eta.vec given lambda2
    eta.post.var <- lambda2.old/D
    eta.vec.new <- rnorm(n=K,mean=eta.post.mean,sd=sqrt(eta.post.var))
    names(eta.vec.new) <- colnames(xi.param.vecs)
    
    # Draw new lambda2 given new eta
    xi.demean.vec <- as.vector(sweep(xi.param.vecs, 2, eta.vec.new, "-"))
    lambda2.df <- D*K
    lambda2.scale <- (1/lambda2.df)*sum(xi.demean.vec^2)
    lambda2.new <- rinvchisq(n=1,df=lambda2.df,scale=lambda2.scale)
  }

  out.list <- list(eta.vec=eta.vec.new)
  
  if(full.Sigma){out.list$Sigma <- Sigma.new
  } else {out.list$lambda2 <- lambda2.new}

  return(out.list)
}
