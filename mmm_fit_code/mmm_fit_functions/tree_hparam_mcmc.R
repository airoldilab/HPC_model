# Script to set up conjugate conditional sampler for tree hyperparameters

library("geoR")

tree.hparam.draw <- function(mu.corpus.vec,gamma2.old,V){

  # Draw new psi
  psi.post.mean <- mean(mu.corpus.vec)
  psi.post.var <- gamma2.old/V
  psi.new <- rnorm(n=1,mean=psi.post.mean,sd=sqrt(psi.post.var))

  # Draw new gamma2
  gamma2.df <- V
  gamma2.scale <- (1/V)*sum((mu.corpus.vec-psi.new)^2)
  gamma2.new <- rinvchisq(n=1,df=gamma2.df,scale=gamma2.scale)
  gamma.new <- sqrt(gamma2.new)

  return(list(psi=psi.new,gamma=gamma.new))
}

