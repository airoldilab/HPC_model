# Want to sample from highly correlated normal

dens.normal <- function(x,mean.vec,Sigma.inv,sig.diag=FALSE){
  deviat.vec <- x - mean.vec
  if(sig.diag){dens <- -0.5*deviat.vec^2*Sigma.inv
  } else {dens <- -0.5*deviat.vec%*%Sigma.inv%*%deviat.vec}
  return(dens)
}

grad.normal <- function(x,mean.vec,Sigma.inv,sig.diag=FALSE){
  deviat.vec <- x - mean.vec
  if(sig.diag){grad <- -Sigma.inv*deviat.vec
  } else {grad <- -Sigma.inv%*%deviat.vec}
  return(grad)
}

U.normal <- function(x){
  out <- -dens.normal(x=x,mean.vec=rep(0,length(x)),Sigma.inv=Sigma.inv)
  return(as.numeric(out))
}

U.normal.grad <- function(x){
  out <- -grad.normal(x=x,mean.vec=rep(0,length(x)),Sigma.inv=Sigma.inv)
  return(as.numeric(out))
}

K.normal <- function(p,Sigma.inv=diag(length(p))){
  out <- -dens.normal(x=p,mean.vec=rep(0,length(p)),Sigma.inv=Sigma.inv)
  return(as.numeric(out))
}

# Draw momentum variables of the same dimension as the position
# sd.M is a vector of standard deviations for momemtum vars
p.draw.diag <- function(x,sd.M=NULL){
  if(is.null(sd.M)){p <- rnorm(n=length(x))        
  } else {p <- rnorm(n=length(x),sd=sd.M)}                      
  return(p)
}

# Draw momentum variables of the same dimension as the position,
# in the case from non-diagonal covariance matrix
p.draw <- function(x,M.chol){
  z <- rnorm(n=length(x))
  p <- as.vector(M.chol%*%z)
  return(p)
}
