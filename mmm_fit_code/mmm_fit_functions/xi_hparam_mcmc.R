# Script to set up conjugate conditional sampler for xi hyperparameters

library("geoR")
library("MCMCpack")
library("mvtnorm")

xi.hparam.draw <- function(xi.param.vecs,D,K,full.Sigma=FALSE,
                           lambda2.old=NULL,Sigma.old=NULL,
                           Sigma.0=NULL,kappa.0=0,omega2.0=0){

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
    Sigma.df <- kappa.0 + D
    Sigma.new <- riwish(v=Sigma.df, S=kappa.0*Sigma.0 + W)
    
  } else {
    
    # Draw new lambda2 given new eta
    # kappa.0==-1 is a flag for a flat prior on log lambda2
    if(kappa.0==-1){
      # Draw lambda2 unconditionally
      xi.demean.vec <- as.vector(sweep(xi.param.vecs, 2, eta.post.mean, "-"))
      lambda2.df <- D*K - 1
      lambda2.scale <- sum(xi.demean.vec^2)/lambda2.df
      lambda2.new <- rinvchisq(n=1,df=lambda2.df,scale=lambda2.scale)
      # Draw eta conditional on the new lambda2
      eta.post.var <- lambda2.new/D
      eta.vec.new <- rnorm(n=K,mean=eta.post.mean,sd=sqrt(eta.post.var))
      names(eta.vec.new) <- colnames(xi.param.vecs)

      # Otherwise have flat prior on lambda2 (kappa.0==0) or proper prior
      # on lambda2 (kappa.0>0)
    } else {
      # Draw new eta.vec given old lambda2
      eta.post.var <- lambda2.old/D
      eta.vec.new <- rnorm(n=K,mean=eta.post.mean,sd=sqrt(eta.post.var))
      names(eta.vec.new) <- colnames(xi.param.vecs)
      # Draw lambda2 given new eta.vec
      xi.demean.vec <- as.vector(sweep(xi.param.vecs, 2, eta.vec.new, "-"))
      lambda2.df <- D*K + kappa.0
      lambda2.scale <- (sum(xi.demean.vec^2)+kappa.0*omega2.0)/lambda2.df
      lambda2.new <- rinvchisq(n=1,df=lambda2.df,scale=lambda2.scale)
    }
  }

  out.list <- list(eta.vec=eta.vec.new)
  
  if(full.Sigma){out.list$Sigma <- Sigma.new
  } else {out.list$lambda2 <- lambda2.new}

  return(out.list)
}
