# General purpose function to turn an importance sampler function into
# indepedence chain Metro-Hastings
metro.sampler <- function(n,import.sampler,par.start=NULL,
                           last.draw=FALSE,...){
  
  # Get draws and weights from importance sampler
  if(is.null(par.start[1])){ndraws <- n+1
  } else {ndraws <- n}
  import.out <- import.sampler(ndraws=ndraws,par.start=par.start,...)
  log.weights <- import.out$log.weights
  #log.weights <- log(import.out$weights)
  draws <- import.out$draws

  # Get log uniform draws for acceptance decisions
  log.unif.draws <- log(runif(n))

  # Get draws from posterior
  accept.draws.pos <- c()
  # Current jumping position in list
  j <- 1
  n.accept <- 0
  for(i in 2:(n+1)){
    # Should jump?
    log.r <- log.weights[i]-log.weights[j]
    jump <- log.unif.draws[i-1] < min(log.r,0)
    if(any(is.na(jump),is.null(jump))){
      print(list(log.r=log.r,jump=jump,log.weights.i=log.weights[i],
                                     log.weights.j=log.weights[j]))}
    # If jump, add new draw to list and advance index
    if(jump){
      accept.draws.pos <- c(accept.draws.pos,i)
      j <- i
      n.accept <- n.accept + 1
    } else {
    # If not going to jump, add old value to list and do not
    # advance index
    accept.draws.pos <- c(accept.draws.pos,j)}
  }

  # Grab accepted draws
  post.draws <- draws[accept.draws.pos,]
  if(last.draw){post.draws=post.draws[nrow(post.draws),]}

  # Get acceptance probability
  accept.rate <- n.accept/n

  metroh.list <- list(draws=post.draws,accept.rate=accept.rate)

  return(metroh.list)
}


# Extra helpful functions

# Function to take Hessian as argument and return Cholesky
# factor of the variance and precision matrix
hessian2sigma <- function(hes,hes.diag=FALSE){

  # Use only diagonal hessian if requested
  if(hes.diag){
    Sigma.inv <- -diag(hes)
    Sigma.inv.chol <- sqrt(Sigma.inv)
    #Sigma <- diag(1/Sigma.inv)
    Sigma.chol <- sqrt(1/Sigma.inv)
    hes.diag <- TRUE

    # Figure out if hessian well behaved, get Sigma.inv derivants
  } else if(all(eigen(-hes)$values>0)){
    Sigma.inv <- -as.matrix(hes)
    Sigma.inv.chol <- chol(Sigma.inv)
    Sigma.chol <- backsolve(Sigma.inv.chol,diag(nrow(Sigma.inv)))
    #Sigma <- t(Sigma.chol)%*%Sigma.chol
    hes.diag <- FALSE
    
  } else {
    # Otherwise need to use diagonal hessian or scalar hessian
    warning("Full hessian numerically indefinite; used diagonal only")
    Sigma.inv <- -diag(hes)
    if(all(Sigma.inv > 0)){
      Sigma.inv.chol <- sqrt(Sigma.inv)
      Sigma.chol <- sqrt(1/Sigma.inv)
    } else {
      ave.diag <- mean(abs(Sigma.inv[Sigma.inv > 0]))
      Sigma.inv.chol <- rep(sqrt(ave.diag),length(Sigma.inv))
      Sigma.chol <- rep(sqrt(1/ave.diag),length(Sigma.inv))
      #Sigma <- rep(1/ave.diag,length(Sigma.inv))
    }
    hes.diag <- TRUE
  }

  if(hes.diag){
    Sigma.chol <- diag(Sigma.chol)
    Sigma.inv.chol <- diag(Sigma.inv.chol)
  }
  

  return(list(Sigma.chol=Sigma.chol,Sigma.inv.chol=Sigma.inv.chol,
              hes.diag=hes.diag))
}


# Function to evaluate normal proposal density
norm.prop.dens <- function(param.vec,mean.vec,Sigma.inv.chol){
  deviat.vec <- param.vec-mean.vec
  ss.sqrt <- Sigma.inv.chol%*%deviat.vec
  log.prop.dens <- -0.5*sum(ss.sqrt^2)
  return(as.numeric(log.prop.dens))
}

# Function to generate proposal draws
norm.prop.draw <- function(ndraws,mean.vec,Sigma.chol){
  dim.vec <- length(mean.vec)
  z.mat <- matrix(rnorm(n=dim.vec*ndraws),nrow=ndraws,ncol=dim.vec)
  norm.vecs <- sweep(z.mat%*%Sigma.chol,MARGIN=2,STATS=mean.vec,FUN="+")
  return(norm.vecs)
}











## # Function to invert hessian matrix
## hessian2sigma <- function(hes){

##     # Check if hes positive definite
##     eigen.hes <- eigen(hes)$values

##     if(!all(eigen.hes>0)){
##       out <- svd(hes)
##       hes <- t(abs(out$d)*t(out$u))%*%t(out$v)
##     }
     
##     z <- chol(hes)

##     # Get positive definite version of hes and hes.inv
##     z.inv <- backsolve(z,diag(nrow(z)))
##     #z.inv <- solve(z)
##     hes.inv <- z.inv%*%t(z.inv)
##     #chol.x <- z
  
##   return(list(hes=hes,hes.inv=hes.inv))
## }


## # Function to invert hessian matrix
## hessian2sigma <- function(hes,check.pd=TRUE){

##   k <- 1
##   if(check.pd){
##     # Check if hes positive definite
##     eigen.hes <- eigen(hes)$values
  
##     # Try to patch up non-positive definite matrix
##     mult <- seq(1,10000000,length.out=10)
##     while(all(!all(eigen.hes>0),k<=10)){
##       if(k==1){hes.orig <- hes}
##       hes <- hes + diag(rep(mult[k]*.Machine$double.eps,ncol(hes)))
##       eigen.hes <- eigen(hes)$values
##       k <- k+1
##     }

##     # Give up after 10 tries and do something sloppy
##     if(k==11){
##       #print(eigen(hes)$values)
##       #stop("Hessian matrix not invertible.")
##       hes <- diag(abs(diag(hes.orig)))
##       hes.inv <- diag(1/abs(diag(hes.orig)))}
##   }

##   if(any(k<11,!check.pd)){
##     # If matrix positive definite, proceed with chol
##     z <- chol(hes)

##     # Get positive definite version of hes and hes.inv
##     z.inv <- backsolve(z,diag(nrow(z)))
##     hes.inv <- z.inv%*%t(z.inv)
##     #chol.x <- z
##   }
  
##   return(list(hes=hes,hes.inv=hes.inv))
## }


  ## # Try to patch up non-positive definite matrix
  ## k <- 1
  ## while(all(z==999,k<=10)){
  ##   x <- x + diag(rep(k*.Machine$double.eps,ncol(x)))
  ##   z <- tryCatch(chol(x),error=function(x){999})
  ##   k <- k+1
  ## }
