
################################################################
######### EXPERIMENTAL SECTION ################################
################################################################

# Trying to integrate out the thetas
require("MCMCpack",quietly=TRUE)


# Function to evaluate log conditional posterior of theta.d
theta.log.like <- function(par,theta.data.list,mlogit=TRUE){
  
  # If making guesses in mlogit space, go back to simplex
  # Note that guesses will be K-1 dimensional due to constraint
  if(mlogit){theta.d <- inv.mlogit(par)
             if(sum(theta.d)>1){return(-Inf)}}
  else{sum.from2 <- sum(par)
       if(sum(par)>1){return(-Inf)}
       else{theta.d <- c(1-sum(par),par)}}
  #print("theta.d:")
  #print(theta.d)

  # Unpack needed data
  active.words <- theta.data.list$active.words
  mu.param.mat <- theta.data.list$mu.param.mat
  X.d <- theta.data.list$X.d
  counts.doc <- theta.data.list$counts.doc
  
  # Get linear predictor
  # For now assuming theta.d a vector of active topics in right order
  x.theta.vec <- as.vector(X.d%*%theta.d)
  names(x.theta.vec) <- rownames(X.d)

  # Evaluate log posterior
  log.like <- -sum(x.theta.vec)+
    sum(counts.doc*log(x.theta.vec[active.words]))

  return(log.like)
}

# Function to evaluate gradient of log conditional posterior of theta.d
# Need to evaulate in mlogit space so that BFGS can guess in
# unconstrained space
theta.log.like.gradient <- function(par,theta.data.list,mlogit=TRUE){

  # If making guesses in mlogit space, go back to simplex
  # Note that guesses in mlogit space will be K-1 dimensional,
  # while guesses in the simplex should be K dimensional.
  if(mlogit){theta.d <- inv.mlogit(par)}
  else{theta.d <- par}

  # Separate out the free parameters from the fixed one
  theta1 <- theta.d[1]
  theta.vec.from2 <- theta.d[-1]

  # Unpack needed data
  active.words <- theta.data.list$active.words
  mu.param.mat <- theta.data.list$mu.param.mat
  X.d <- theta.data.list$X.d
  counts.doc <- theta.data.list$counts.doc
  
  # Get linear predictor
  # For now assuming theta.d a vector of active topics in right order
  x.theta.vec <- as.vector(X.d%*%theta.d)
  names(x.theta.vec) <- rownames(X.d)
  
  # Get count ratio for active words
  count.ratio <- counts.doc/x.theta.vec[active.words]

  # Get differences from baseline topic sums
  X.d.from2 <- X.d[,-1]
  if(is.vector(X.d.from2)){X.d.from2 <- as.matrix(X.d.from2)}
  X.diff.mat <- apply(X.d.from2,2,function(col){col-X.d[,1]})
  
  # Get column sums of X.d
  X.diff.col.sums <- colSums(X.diff.mat)

  # Get gradient
  gradient <- -X.diff.col.sums +
    as.vector(count.ratio%*%X.diff.mat[active.words,])

  # If mlogit guesses, use chain rule to get gradient in x space
  if(mlogit){
    # Get sum of theta.vec.from2 leaving each one out
    theta.sum.vec <- sum(theta.vec.from2)-theta.vec.from2
    chain.rule <- theta.vec.from2*(1-theta.vec.from2) -
      theta.vec.from2*theta.sum.vec
    gradient <- gradient*chain.rule
  }

  return(gradient)
  
}
  

optim.theta.like <- function(job.id,current.param.list,doc.length.vec,
                             doc.topic.list,doc.count.list,
                             topic.address.book){

  # Get old theta.d from last update
  theta.d.raw <- current.param.list$theta.param.vecs[job.id,]
  pos.active <- which(theta.d.raw>0)
  theta.d.old <- theta.d.raw[pos.active]
  #print(theta.d.old)

  # Is there only two active topics? If so, line search needed instead
  is.two.active <- length(theta.d.old)==2
  if(is.two.active){theta.start <- theta.d.old[-1]}
  else{theta.start <- mlogit(theta.d.old)}

  # Construct data needed for optimization
  theta.data.list <- get.data.for.theta(doc.id=job.id,
                                        mu.param.vecs=current.param.list$mu.param.vecs,
                                        doc.length.vec=doc.length.vec,
                                        doc.topic.list=doc.topic.list,
                                        count.list=doc.count.list,
                                        topic.address.book=topic.address.book,
                                        alpha=current.param.list$alpha)


  # If only two active topics, use simple line search to ensure stability
  if(is.two.active){
    optim.out <- optimize(f=theta.log.like,interval=c(0,1),
                          maximum=TRUE,theta.data.list=theta.data.list,
                          mlogit=FALSE)
    theta.2 <- optim.out$maximum
    theta.d.new <- c(1-theta.2,theta.2)
    theta.d.raw.new <- theta.d.raw
    theta.d.raw.new[pos.active] <- theta.d.new
  }
  
  # If more than two active topics, optimize log like with BFGS
  else{optim.out <- optim(par=theta.start,fn=theta.log.like,
                          control=list(fnscale=-1),
                          theta.data.list=theta.data.list,
                          #method="BFGS",
                          method="L-BFGS-B",
                          upper=c(700,700),lower=c(-745,-745),
                          gr=theta.log.like.gradient)
       theta.d.new <- inv.mlogit(optim.out$par)
       theta.d.raw.new <- theta.d.raw
       theta.d.raw.new[pos.active] <- theta.d.new
     }

  # Check global convergence of theta.d
  global.conv <- check.conv(old.param.vec=theta.d.old,
                            new.param.vec=theta.d.new)

  out.list <- list(theta.d=theta.d.raw.new,global.conv=global.conv)
}


# Get best alpha.0 for Dirichlet approx to likelihood
get.alpha.0 <- function(mean.vec,theta.data.list){

  # First get variance of each theta.dk:
  theta.d <- mean.vec
  
  # Separate out the free parameters from the fixed one
  theta1 <- theta.d[1]
  theta.vec.from2 <- theta.d[-1]

  # Unpack needed data
  active.words <- theta.data.list$active.words
  mu.param.mat <- theta.data.list$mu.param.mat
  X.d <- theta.data.list$X.d
  counts.doc <- theta.data.list$counts.doc
  
  # Get linear predictor
  # For now assuming theta.d a vector of active topics in right order
  x.theta.vec <- as.vector(X.d%*%theta.d)
  names(x.theta.vec) <- rownames(X.d)
  
  # Get count ratio for active words
  count.ratio <- counts.doc/x.theta.vec[active.words]^2

  # Get differences from baseline topic sums
  X.d.from2 <- X.d[,-1]
  if(is.vector(X.d.from2)){X.d.from2 <- as.matrix(X.d.from2)}
  X.diff.mat <- apply(X.d.from2,2,function(col){col-X.d[,1]})

  # Get vector of variances and calculate its mean
  var.vec <- 1/as.vector(count.ratio%*%(X.diff.mat[active.words,]^2))
  mean.var.like <- mean(var.vec)

  # Now get numerator
  numer <- mean(mean.vec*(1-mean.vec))

  alpha.0 <- (numer/mean.var.like) - 1
  
  return(alpha.0)
}

# Get average of two log densities
# Big problems if one or both densities so big that overflow
# or if both underflow.
# If one overflows and one underflows less of a big deal
# Mix.prob is the weight for the first density
ave.log.dens <- function(mix.prob,log.dens1,log.dens2){
  # Check to see if either density will overflow
  overflow <- any(exp(log.dens1)==Inf,exp(log.dens2)==Inf)
  
  # Check to see if both densities underflow
  underflow <- all(exp(log.dens1)==0,exp(log.dens2)==0)
  
  if(overflow){
    # Here assuming that 700 the boundary for overflow
    # Could subtract more, but danger of other number underflowing
    log.const <- max(log.dens1,log.dens2)-700
    ave.log.dens <- log(mix.prob*exp(log.dens1-log.const) +
                        (1-mix.prob)*exp(log.dens2-log.const)) +
                          log.const
  }

  else if(underflow){
    # Here since both underflow, can add as much as we want to get to zero
    log.const <- -min(log.dens1,log.dens2)
    ave.log.dens <- log(mix.prob*exp(log.dens1+log.const) +
                        (1-mix.prob)*exp(log.dens2+log.const)) -
                          log.const
  }

  # Otherwise just do naive averaging
  else{
    ave.log.dens <- log(mix.prob*exp(log.dens1) +
      (1-mix.prob)*exp(log.dens2))
  }

  return(ave.log.dens)
}

dirichlet.dens <- function(x,alpha,log) {
  logD <- sum(lgamma(alpha)) - lgamma(sum(alpha))
  s <- sum((alpha - 1) * log(x))
  out <- sum(s) - logD
  if(!log){out <- exp(out)}
  return(out)
}

ddirichlet.fast <- function(x, alpha, log=FALSE){
  if (!is.matrix(x)) 
    if (is.data.frame(x)) 
      x <- as.matrix(x)
    else x <- t(x)
  if (!is.matrix(alpha)) 
    alpha <- matrix(alpha, ncol = length(alpha), nrow = nrow(x), 
                    byrow = TRUE)
  if (any(dim(x) != dim(alpha))) 
    stop("Mismatch between dimensions of x and alpha in ddirichlet().\n")
  #pd <- vector(length = nrow(x))
  pd <- apply(x,1,dirichlet.dens,alpha=alpha,log=log)
  #pd <- t(sapply(1:nrow(x),function(row.pos){dirichlet.dens(x[row.pos,],alpha,log)}))
  pd[apply(x, 1, function(z) any(z < 0 | z > 1))] <- 0
  pd[apply(x, 1, function(z) all.equal(sum(z), 1) != TRUE)] <- 0
  return(pd)
}


# Function to implement indepedent draw Metro-Hastings for thetas
metroh.sampler.theta <- function(n,MLE.vec,theta.data.list){
  
  alpha.0 <- get.alpha.0(mean.vec=MLE.vec,theta.data.list=theta.data.list)
  like.alpha <- alpha.0*MLE.vec

  # Get samples from both distributions
  draws <- rdirichlet(n=n-1,alpha=like.alpha)
  draws <- rbind(MLE.vec,draws)

  # Get log uniform draws for acceptance decisions
  log.unif.draws <- log(runif(n))

  # Get importance weights of each draw
  draws.post.log.dense <- apply(as.matrix(draws[,-1]),
                               1,theta.log.posterior,
                               theta.data.list=theta.data.list,
                               mlogit=FALSE)
  
  draws.prop.log.dense <- apply(draws,1,ddirichlet.fast,alpha=like.alpha,log=TRUE)

  log.weights <- draws.post.log.dense-draws.prop.log.dense

  # Get draws from posterior
  post.draws <- MLE.vec
  # Current jumping position in list
  j <- 1
  n.accept <- 0
  for(i in 2:n){
    # Should jump?
    log.r <- log.weights[i]-log.weights[j]
    jump <- log.unif.draws[i] < min(log.r,0)
    # If jump, add new draw to list and advance index
    if(jump){
      post.draws <- rbind(post.draws,draws[i,])
      j <- i
      n.accept <- n.accept + 1
    }
    # If not going to jump, add old value to list and do not
    # advance index
    else{post.draws <- rbind(post.draws,draws[j,])}
  }

  # Get acceptance probability
  accept.prob <- n.accept/(n-1)

  metroh.list <- list(draws=post.draws,accept.prob=accept.prob)

  return(metroh.list)
}


# Function to implement importance sampler for thetas
import.sampler.theta <- function(n,prob.like,prior.alpha,alpha.0,MLE.vec,
                                 theta.data.list){

  like.alpha <- alpha.0*MLE.vec

  # Figure out how many to sample from each distribution
  if(prob.like==1){n.like <- n}
  else{n.like <- rbinom(1,n,prob.like)
       n.prior <- n-n.like}

  # Get samples from both distributions
  like.draws <- rdirichlet(n=n.like,alpha=like.alpha)
  if(prob.like<1){
  prior.draws <- rdirichlet(n=n.prior,alpha=prior.alpha)
  draws <- rbind(like.draws,prior.draws)}
  else{draws <- like.draws}

  # Get importance weights of each draw
  draws.post.log.dense <- apply(as.matrix(draws[,-1]),
                               1,theta.log.posterior,
                               theta.data.list=theta.data.list,
                               mlogit=FALSE)
  
  draws.prop.log.dense1 <- apply(draws,1,ddirichlet.fast,alpha=like.alpha,log=TRUE)
                                        #ddirichlet.fast(x=draws,alpha=like.alpha,log=TRUE)
                                        #apply(draws,1,ddirichlet.fast,alpha=like.alpha,log=TRUE)
  if(prob.like<1){
    draws.prop.log.dense2 <- apply(draws,1,ddirichlet.fast,
                                   alpha=prior.alpha,log=TRUE)
    draws.prop.log.dense <- mapply(ave.log.dense,log.dense1=draws.prop.dense1,
                                   log.dense2=draws.prop.dense2,
                                   MoreArgs=list(mix.prob=prob.like))}
  else{draws.prop.log.dense <- draws.prop.log.dense1}

  #browser()
  log.weights <- draws.post.log.dense-draws.prop.log.dense

  weights <- exp(log.weights-mean(log.weights))

  imptsamp.list <- list(draws=draws,weights=weights)
  return(imptsamp.list)
  
}



