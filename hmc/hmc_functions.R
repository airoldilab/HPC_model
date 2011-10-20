# Core functions for HMC sampling

# U is the potential energy function---the negative log posterior
# U.grad is the gradient of U wrt the parameters
# K is the potential energy function
# M is the mass matrix (generally inverse curvature)
# nsteps is the number of leapfrog steps
# step.size is the leapfrog step size


# Draw momentum variables of the same dimension as the position
# M.sd is either a vector of standard deviations or
# a cholesky factor (depending on M.diag)
p.draw <- function(M.sd,M.diag){
  if(M.diag){p <- rnorm(n=length(M.sd),sd=M.sd)
  } else {
    z <- rnorm(n=nrow(M.sd))
    p <- as.vector(M.sd%*%z)
  }
  return(p)
}

# Function to perform leapfrog steps
leapfrog <- function(U.grad,q.current,p.current,step.size,nsteps,U,
                     M.inv,M.diag,debug,...){

  if(debug){
    # Record p and q along the way if debugging
    p.steps <- p.current
    q.steps <- q.current
    h.steps <- hamil(q=q.current,p=p.current,U=U,
                     M.inv=M.inv,M.diag=M.diag,...)
  }

  # If only one step requested, save time with modified Euler method
  if(nsteps==1){
    p.current <- p.current - step.size*U.grad(q.current,...)
    if(M.diag){q.current <- q.current + step.size*M.inv*p.current
    } else {q.current <- q.current + step.size*as.vector(M.inv%*%p.current)}
  } else {
  # Otherwise, use more intensive leapfrog method to reduce integration error
  # Debug mode only active for leapfrog steps 
    for(i in 1:nsteps){ 
      half.momentum <- p.current - (step.size/2)*U.grad(q.current,...)
      if(M.diag){q.current <- q.current + step.size*M.inv*half.momentum
      } else {q.current <- q.current + step.size*as.vector(M.inv%*%half.momentum)}
      #q.current <- q.current + step.size*as.vector(M.inv%*%half.momentum)
      p.current <- half.momentum - (step.size/2)*U.grad(q.current,...)
      if(debug){
        h.current <- hamil(q=q.current,p=p.current,U=U,
                           M.inv=M.inv,M.diag=M.diag,...)
        p.steps <- rbind(p.steps,p.current)
        q.steps <- rbind(q.steps,q.current)
        h.steps <- c(h.steps,h.current)
      }
    }
    if(debug){browser()}
  }

  new.q <- q.current  
  new.p <- p.current

  out.list <- list(q=new.q, p=new.p)
  if(debug){
    out.list$q.steps <- q.steps
    out.list$q.steps <- p.steps
    out.list$q.steps <- h.steps
  }

  return(out.list)
}


# Function to evaluate Hamiltonian at q*,p*
hamil <- function(q,p,U,M.inv,M.diag,...){
  U.step <- U(q,...)
  K.step <- K.normal(p=p,M.inv=M.inv,M.diag=M.diag)
  h.out <- U.step + K.step
  # Print list of contents if don't have finite density evaluations
  # and set h.out to Inf so move rejected
  if(any(is.na(h.out),is.nan(h.out))){
    warning("HMC move produced NA or NaN Hamiltonian; energy set to Inf")
    if(M.diag){M.inv.diag <- M.inv
    } else {M.inv.diag <- diag(M.inv)}
    #print(list(q=q,p=p,U.step=U.step,K.step=K.step,
    #           M.inv.diag=M.inv.diag,M.diag=M.diag))
    h.out <- Inf
  }
  return(h.out)
}


hmc.prop <- function(q.current,U,U.grad,M.sd,M.inv,M.diag,
                     step.size,nsteps,debug,...){
  
  # Draw momentum variables
  p.start <- p.draw(M.sd=M.sd,M.diag=M.diag)
  p.current <- p.start
  
  # Run leapfrog steps to simulate dynamics
  lf.out <- leapfrog(U.grad=U.grad,q.current=q.current,
                     p.current=p.current,step.size=step.size,
                     nsteps=nsteps,U=U,M.inv=M.inv,
                     M.diag=M.diag,debug=debug,...)

  # Negate momentum at end of trajectory to make the proposal symmetric
  p.cand <- -lf.out$p
  q.cand <- lf.out$q

  # Evaluate potential and kinetic energies at start and end of trajectory
  h.current <- hamil(q=q.current,p=p.current,U=U,M.inv=M.inv,M.diag=M.diag,...)
  h.cand <- hamil(q=q.cand,p=p.cand,U=U,M.inv=M.inv,M.diag=M.diag,...)
  
  # Decide whether to accept or reject candidate
  # If both energies are infinite, then just reject
  if(all(h.current==Inf,h.cand==Inf)){ r <- -Inf
  } else {r <- h.current-h.cand}
  if(log(runif(1)) < r){ q.new <- q.cand  
  } else {q.new <- q.current}
  
  return(q.new)
}


# Function to take 'ndraws' HMC steps
# All arguments to '...' passed to U and U.grad functions
metro.hmc <- function(ndraws,q.start,nsteps,step.size,
                      U,U.grad,M.sd,M.inv,M.diag,
                      debug=FALSE,...){
  
  state.mat <- q.start
  q.current <- q.start

  # Make M.sd a sparse matrix to speed computation
  if(!M.diag){M.sd <- as(M.sd,"sparseMatrix")}
  
  for(i in 1:ndraws){
    # Get new draw
    q.new <- hmc.prop(q.current=q.current,U=U,U.grad=U.grad,
                      M.sd=M.sd,M.inv=M.inv,M.diag=M.diag,
                      step.size=step.size,nsteps=nsteps,
                      debug=debug,...)
    
    # Store draw
    state.mat <- rbind(state.mat,q.new)
    q.current <- q.new
    
  }
  
  return(state.mat)
}



##############################################################
# Function to create a mass matrix from an estimated Hessian #
##############################################################

# In this function:
# M is the mass matrix
# M.sd is the Cholesky factor of the mass matrix
# M.inv is the inverse of the mass matrix, or the covariance
#    of the particle's masses
# M.diag is a flag for whether the output is a vector (the
#   diagonal of the above elements) or matrix

hes2mass <- function(hes,hes.diag=FALSE){
  
  # Use only diagonal mass matrix if hessian diagonal
  if(hes.diag){
    M <- -hes
    M.sd <- sqrt(M)
    M.inv <- 1/M
    M.diag <- TRUE

  # Otherwise get full Hessian
  } else {
    # Figure out if hessian needs modification to be p.d.
    if(!all(eigen(-hes)$values>0)){
      hes <- make.hes.pd(hes)
      warning("Full hessian numerically indefinite and required modification to be p.d.")
    }
    # Calcualte mass matrix and needed derivants
    M <- -as.matrix(hes)
    M.sd <- t(chol(M))
    M.inv.chol <- forwardsolve(M.sd,diag(nrow(M)))
    M.inv <- t(M.inv.chol)%*%M.inv.chol
    M.diag <- FALSE
  }

  return(list(M.inv=M.inv,M.sd=M.sd,M.diag=M.diag))
}


## hes2mass <- function(hes,hes.diag=FALSE){
  
##   # Use only diagonal mass matrix if hessian diagonal
##   if(hes.diag){
##     M <- -hes
##     M.sd <- sqrt(M)
##     M.inv <- 1/M
##     M.diag <- TRUE

##     # Figure out if hessian well behaved, get M derivants
##   } else if(all(eigen(-hes)$values>0)){
##     M <- -as.matrix(hes)
##     M.sd <- t(chol(M))
##     M.inv.chol <- forwardsolve(M.sd,diag(nrow(M)))
##     M.inv <- t(M.inv.chol)%*%M.inv.chol
##     M.diag <- FALSE
    
##   } else {
##     # Otherwise need to use diagonal hessian or scalar hessian
##     warning("Full hessian numerically indefinite; used diagonal only")
##     M <- -diag(hes)
##     if(all(M>0)){
##       M.sd <- sqrt(M)
##       M.inv <- 1/M
##     } else {
##       M[M < 0] <- min(M[M > 0])
##       M.sd <- sqrt(M)
##       M.inv <- 1/M
##       print("Diagonal approximation mass matrix: ")
##       print(M)
##     }
##     M.diag <- TRUE
##   }

##   return(list(M.inv=M.inv,M.sd=M.sd,M.diag=M.diag))
## }


# Function to modify non-positive definite Hessian
# Algorithm from Nocedal and Wright (1999, p. 145)
make.hes.pd <- function(hes){
  # Get Frobenius norm of negative hes to get optimal modification
  nhes <- -hes
  fnorm.hes <- sqrt(sum(nhes^2))
  delta <- fnorm.hes/2
  is.pd <- FALSE

  # Continue to add multiples of modification until hes is p.d.
  while(!is.pd){
    diag(nhes) <- diag(nhes) + delta
    delta <- 2*delta
    if(all(eigen(nhes)$values>0)){is.pd <- TRUE}
  }

  return(-nhes)
}

###############################################################
# Some default functions for Gaussian distributions in U or K #
###############################################################

# If sig.diag=TRUE, assume Sigma.inv is a vector
dens.normal <- function(x,mean.vec,Sigma.inv,sig.diag=FALSE){
  deviat.vec <- x - mean.vec
  if(sig.diag){dens <- -0.5*sum(deviat.vec^2*Sigma.inv)
  } else {dens <- -0.5*deviat.vec%*%Sigma.inv%*%deviat.vec}
  return(as.numeric(dens))
}

grad.normal <- function(x,mean.vec,Sigma.inv,sig.diag=FALSE){
  deviat.vec <- x - mean.vec
  if(sig.diag){grad <- -Sigma.inv*deviat.vec
  } else {grad <- -as.vector(Sigma.inv%*%deviat.vec)}
  return(grad)
}

U.normal <- function(q,Sigma.inv){
  out <- -dens.normal(x=q,mean.vec=rep(0,length(q)),Sigma.inv=Sigma.inv)
  return(out)
}

U.normal.grad <- function(q,Sigma.inv){
  out <- -grad.normal(x=q,mean.vec=rep(0,length(q)),Sigma.inv=Sigma.inv)
  return(out)
}

K.normal <- function(p,M.inv,M.diag=FALSE){
  out <- -dens.normal(x=p,mean.vec=rep(0,length(p)),Sigma.inv=M.inv,
                      sig.diag=M.diag)
  return(as.numeric(out))
}
