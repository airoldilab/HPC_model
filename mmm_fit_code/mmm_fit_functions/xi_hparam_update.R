library("geoR")
library("MCMCpack")
library("mvtnorm")

lambda2.like.fd <- function(xi.param.vecs,D,K,eta.vec,lambda2){
  xi.ss <- sum(as.vector(sweep(xi.param.vecs, 2, eta.vec, "-")))
  fd <- -(D*K/2)*lambda2^(-1) + 0.5*lambda2^(-2)*xi.ss
  return(fd)
}

lambda2.like.sd <- function(xi.param.vecs,D,K,eta.vec,lambda2){
  xi.ss <- sum(as.vector(sweep(xi.param.vecs, 2, eta.vec, "-")))
  sd <- (D*K/2)*lambda2^(-2) - lambda2^(-3)*xi.ss
  return(sd)
}

lambda2.log.prior <- function(omega2,kappa,lambda2){
  if(kappa==-1){log.prior <- -log(lambda2)
  } else {
    log.prior <- -(1+(kappa/2))*log(lambda2) - 0.5*kappa*omega2*lambda2^(-1)
  }
  return(log.prior)
}

lambda2.prior.fd <- function(omega2,kappa,lambda2){
  if(kappa==-1){fd <- -lambda2^(-1)
  } else {
    fd <- -(1+(kappa/2))*lambda2^(-1) + 0.5*kappa*omega2*lambda2^(-2)
  }
  return(fd)
}

lambda2.prior.sd <- function(omega2,kappa,lambda2){
  if(kappa==-1){sd <- lambda2^(-2)
  } else {
    sd <- (1+(kappa/2))*lambda2^(-2) - kappa*omega2*lambda2^(-3)
  }
  return(sd)
}

lambda2.post.fd <- function(xi.param.vecs,D,K,eta.vec,lambda2,
                            omega2,kappa){
 fd <- lambda2.like.fd(xi.param.vecs=xi.param.vec,D=D,K=K,
                    eta.vec=eta.vec,lambda2=lambda2) +
                   lambda2.prior.fd(omega2=omega2,
                    kappa=kappa,lambda2=lambda2)
 return(fd)
}

lambda2.post.sd <- function(xi.param.vecs,D,K,eta.vec,lambda2,
                            omega2,kappa){
  sd <- lambda2.like.sd(xi.param.vecs=xi.param.vecs,D=D,K=K,
                        eta.vec=eta.vec,lambda2=lambda2) +
          lambda2.prior.sd(omega2=omega2,kappa=kappa,lambda2=lambda2)
  return(sd)
}

eta.vec.like.fd <- function(xi.param.vecs,D,K,eta.vec,lambda2){
  xi.demean.mat <- sweep(xi.param.vecs, 2, eta.post.mean, "-")
  xi.colsum.demean <- colSums(xi.demean.mat)
  fd <- lambda2^(-1)*xi.colsum.demean
  return(fd)
}

eta.vec.like.sd <- function(D,K,lambda2){
  sd <- rep(-D*lambda2^(-1),K)
  return(sd)
}

# Only off-diagonals involving lambda2 are non-zero
# Note that off-diags the same for post and like since priors fall out
eta.lambda2.like.sd <- function(xi.param.vecs,D,K,eta.vec,lambda2){
  xi.demean.mat <- sweep(xi.param.vecs, 2, eta.post.mean, "-")
  xi.colsum.demean <- colSums(xi.demean.mat)
  sd <- -lambda2^(-2)*xi.colsum.demean
  return(sd)
}


xi.hparam.log.like <- function(par,xi.param.vecs,D,K){
  lambda2 <- exp(par[1])
  eta.vec <- par[-1]
  xi.ss <- sum(as.vector(sweep(xi.param.vecs, 2, eta.vec, "-")))
  log.like <- -(D*K/2)*log(lambda2) - 0.5*lambda2^(-1)*xi.ss
  return(log.like)
}

xi.hparam.post <- function(par,xi.param.vecs,D,K,omega2,kappa){
  post <- xi.hparam.log.like(par=par,xi.param.vecs=xi.param.vecs,
                             D=D,K=K) +
            lambda2.log.prior(omega2=omega2,kappa=kappa,lambda2=lamba2)
  return(post)
}

xi.hparam.post.grad <- function(par,xi.param.vecs,D,K,omega2,kappa){
  lambda2 <- exp(par[1])
  eta.vec <- par[-1]

  # First deriv wrt lambda2
  lambda2.fd <- lambda2.post.fd(xi.param.vecs=xi.param.vec,D=D,K=K,
                                eta.vec=eta.vec,lambda2=lambda2,
                                omega2=omega2,kappa=kappa)

  # Transform to log space
  log.lambda2.fd <- lambda2.fd*lambda2

  # First deriv wrt eta.vec
  eta.vec.fd <- eta.vec.like.fd(xi.param.vecs=xi.param.vecs,D=D,K=K,
                                eta.vec=eta.vec,lambda2=lambda2)

  grad <- c(log.lambda2.fd,eta.vec.fd)

  return(grad)
}


xi.hparam.post.hessian <- function(lambda2,eta.vec,xi.param.vecs,
                                   D,K,omega2,kappa){

  lambda2.fd <- lambda2.post.fd(xi.param.vecs=xi.param.vec,D=D,K=K,
                                eta.vec=eta.vec,lambda2=lambda2,
                                omega2=omega2,kappa=kappa)
  lambda2.sd <- lambda2.post.sd(xi.param.vecs=xi.param.vec,D=D,K=K,
                                eta.vec=eta.vec,lambda2=lambda2,
                                omega2=omega2,kappa=kappa)
  eta.vec.sd <- eta.vec.like.sd(D=D,K=K,lambda2=lambda2)
  eta.lambda2.sd <- eta.lambda2.like.sd(xi.param.vecs,D,K,eta.vec,lambda2)

  h.lambda2 <- lambda2*lambda2.fd + lambda2^2*lambda2.sd
  h.eta.vec <- eta.vec.sd
  h.lambda2.eta.vec <- lambda2*eta.lambda2.sd

  hessian <- diag(c(h.lambda2,h.eta.vec))
  pos.update <- c(1:K) + 1
  hessian[1,pos.update] <- h.lambda2.eta.vec
  hessian[pos.update,1] <- h.lambda2.eta.vec
  
  rownames(hessian) <- colnames(hessian) <- c("lambda2",names(eta.vec))
  
  return(hessian)
}


profile.optim.xi.hparam <- function(xi.param.vecs){
  eta.vec.max <- colMeans(xi.param.vecs)
  
  
}
