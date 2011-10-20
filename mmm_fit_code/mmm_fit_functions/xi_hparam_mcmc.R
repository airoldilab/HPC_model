# Script to set up conjugate conditional sampler for xi hyperparameters

library("geoR")

xi.hparam.draw <- function(xi.param.vecs,lambda2.old,D,K){

  # Draw new eta.vec
  eta.post.mean <- colMeans(xi.param.vecs)
  eta.post.var <- lambda2.old/D
  eta.vec.new <- rnorm(n=K,mean=eta.post.mean,sd=sqrt(eta.post.var))
  names(eta.vec.new) <- colnames(xi.param.vecs)

  # Draw new lambda2
  xi.demean.vec <- as.vector(sweep(xi.param.vecs, 2, eta.vec.new, "-"))
  lambda2.df <- D*K
  lambda2.scale <- (1/lambda2.df)*sum(xi.demean.vec^2)
  lambda2.new <- rinvchisq(n=1,df=lambda2.df,scale=lambda2.scale)

  return(list(eta.vec=eta.vec.new,lambda2=lambda2.new))
}
