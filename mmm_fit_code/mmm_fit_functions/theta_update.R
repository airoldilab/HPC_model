# Functions to update theta vector in mixed membership model

#source("check.conv.R")

# If K topics, going to make guesses in R^(K-1) and use the multivariate
# logit transformation to get a K-1 simplex
# Function to go from theta to x
mlogit <- function(theta.vec){
  theta1 <- theta.vec[1]
  theta.vec.from2 <- theta.vec[-1]
  x.vec <- log(theta.vec.from2)-log(theta1)
  return(x.vec)
}
# Function to go from x to theta
inv.mlogit <- function(x.vec){
  esum <- sum(exp(x.vec))
  theta.vec.from2 <- exp(x.vec)/(1+esum)
  theta1 <- 1/(1+esum)
  theta <- c(theta1,theta.vec.from2)
  return(theta)
}

# Function to get params and data in useful format (anything that
# doesn't involve theta and should not be recalculated)
get.data.for.theta <- function(doc.id,mu.param.vecs,doc.length.vec,
                               doc.count.list,topic.address.book,
                               alpha,doc.topic.list=NULL,
                               classify=FALSE){

  # Make sure doc.id refers to id and not position
  doc.id <- toString(doc.id)
  
  # Get full list of topics
  topics <- topic.address.book[,"topic"]
  
  # Get doc specific objects
  if(!classify){topics.doc <- doc.topic.list[[doc.id]]}
  doc.norm.length <- doc.length.vec[doc.id]
  counts.doc <- doc.count.list[[doc.id]]
  
  # Active topics and words
  if(!classify){active.topics <-
                  sapply(topics.doc,function(topic){which(topics==topic)})}
  active.words <- names(counts.doc)
  
  # Create data
  if(classify){alpha.mult <- alpha-1}
  else{alpha.mult <- alpha[active.topics]-1}
  
  # mu.param.vecs is FxK
  # Only want mus from active topics
  if(classify){mu.param.mat <- mu.param.vecs}
  else{mu.param.mat <- mu.param.vecs[,active.topics]}
  X.d <- doc.norm.length*exp(mu.param.mat)

  theta.data.list <- list(counts.doc=counts.doc,
                          active.words=active.words,alpha.mult=alpha.mult,
                          mu.param.mat=mu.param.mat,X.d=X.d)

  if(!classify){theta.data.list$active.topics <- active.topics}

  return(theta.data.list)
}


# Function to evaluate log conditional posterior of theta.d
theta.log.posterior <- function(par,theta.data.list,mlogit=TRUE,
                                penalty=NULL,debug=FALSE,
                                enforce.constr=TRUE){
  
  # If making guesses in mlogit space, go back to simplex
  # Note that guesses will be K-1 dimensional due to constraint
  if(mlogit){if(debug){print(par)}
             theta.d <- inv.mlogit(par)
             if(all(enforce.constr,sum(par)>1)){return(-Inf)}
             # Make sure no theta actually zero
             if(any(theta.d==0)){
               # Machine percision limit for thetas
               log.lim <- 0.4*log(.Machine$double.eps)
               theta.d[theta.d==0] <- exp(log.lim)}
             if(debug){print(theta.d)}
   } else {sum.from2 <- sum(par)
           if(debug){print(par)}
           if(all(enforce.constr,sum(par)>1)){return(-Inf)}
           theta.d <- c(1-sum(par),par)
           # Make sure no theta actually zero
           if(any(theta.d==0)){
             # Machine percision limit for thetas
             log.lim <- 0.4*log(.Machine$double.eps)
             theta.d[theta.d==0] <- exp(log.lim)}
           if(debug){print(theta.d)} 
         }

  # Unpack needed data
  active.words <- theta.data.list$active.words
  alpha.mult <- theta.data.list$alpha.mult
  mu.param.mat <- theta.data.list$mu.param.mat
  X.d <- theta.data.list$X.d
  counts.doc <- theta.data.list$counts.doc
  
  # Get linear predictor
  # For now assuming theta.d a vector of active topics in right order
  x.theta.vec <- as.vector(X.d%*%theta.d)
  names(x.theta.vec) <- rownames(X.d)

  # Evaluate log posterior
  log.posterior <- -sum(x.theta.vec)+
    sum(counts.doc*log(x.theta.vec[active.words]))+
      sum(alpha.mult*log(theta.d))
  if(!is.null(penalty)){
    log.posterior <- log.posterior - penalty*(sum(theta.d)-1)^2
  }
  if(debug){print(log.posterior)}

  return(log.posterior)
}

# Function to evaluate gradient of log conditional posterior of theta.d
# Need to evaulate in mlogit space so that BFGS can guess in
# unconstrained space
theta.log.post.gradient <- function(par,theta.data.list,mlogit=TRUE,
                                    debug=FALSE,enforce.constr=TRUE){

  # If making guesses in mlogit space, go back to simplex
  # Note that guesses in mlogit space will be K-1 dimensional,
  # while guesses in the simplex should be K dimensional.
  if(mlogit){theta.d <- inv.mlogit(par)
           } else {theta.d <- par}
  # Make sure no theta actually zero
  # Machine percision limit for thetas
  log.lim <- 0.4*log(.Machine$double.eps)
  if(any(theta.d==0)){theta.d[theta.d==0] <- exp(log.lim)}

  # Separate out the free parameters from the fixed one
  theta1 <- theta.d[1]
  theta.vec.from2 <- theta.d[-1]

  # Unpack needed data
  active.words <- theta.data.list$active.words
  alpha.mult <- theta.data.list$alpha.mult
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
    as.vector(count.ratio%*%X.diff.mat[active.words,]) -
    (alpha.mult[1]/theta1)*theta.vec.from2 + alpha.mult[-1]/theta.vec.from2

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
  

optim.theta <- function(job.id,current.param.list,doc.length.vec,
                        doc.count.list,topic.address.book,
                        doc.topic.list=NULL,debug=FALSE,
                        enforce.constr=TRUE,classify=FALSE){

  # Get old theta.d from last update
  theta.d.raw <- current.param.list$theta.param.vecs[job.id,]
  pos.active <- which(theta.d.raw > 0)
  theta.d.old <- theta.d.raw[pos.active]


  # Is there only two active topics? If so, line search needed instead
  is.two.active <- length(theta.d.old)==2
  if(is.two.active){theta.start <- theta.d.old[-1]}
  else{theta.start <- mlogit(theta.d.old)}

  # Construct data needed for optimization
  theta.data.list <- get.data.for.theta(doc.id=job.id,
                                        mu.param.vecs=
                                        current.param.list$mu.param.vecs,
                                        doc.length.vec=doc.length.vec,
                                        doc.topic.list=doc.topic.list,
                                        doc.count.list=doc.count.list,
                                        topic.address.book=topic.address.book,
                                        alpha=current.param.list$alpha,
                                        classify=classify)


  # Evaluate posterior at old values
  post.old <- theta.log.posterior(theta.d.old[-1],
                                  theta.data.list=theta.data.list,
                                  mlogit=FALSE)

  # Machine percision limit for thetas
  log.lim <- 0.4*log(.Machine$double.eps)

  # If only two active topics, use simple line search to ensure stability
  if(is.two.active){
    optim.out <- optimize(f=theta.log.posterior,
                          interval=c(exp(log.lim),1-exp(log.lim)),
                          maximum=TRUE,theta.data.list=theta.data.list,
                          mlogit=FALSE,enforce.constr=enforce.constr)
    theta.2 <- optim.out$maximum
    theta.d.new <- c(1-theta.2,theta.2)
    theta.d.raw.new <- theta.d.raw
    theta.d.raw.new[pos.active] <- theta.d.new

    
  } else {
    # If more than two active topics, optimize log posterior with BFGS
    optim.out <- optim(par=theta.start,fn=theta.log.posterior,
                          control=list(fnscale=-1),
                          theta.data.list=theta.data.list,
                          #method="BFGS",
                          method="L-BFGS-B",
                          upper=-log.lim,lower=log.lim,
                          #upper=700,lower=-745,
                          gr=theta.log.post.gradient,
                          debug=debug,enforce.constr=enforce.constr)
       theta.d.new <- inv.mlogit(optim.out$par)
       # Don't want to return theta with actual zero
       if(any(theta.d.new==0)){theta.d.new[theta.d.new==0] <- exp(log.lim)}
       theta.d.raw.new <- theta.d.raw
       theta.d.raw.new[pos.active] <- theta.d.new
     }

  # Evaluate posterior at new values
  post.new <- theta.log.posterior(theta.d.new[-1],
                                  theta.data.list=theta.data.list,
                                  mlogit=FALSE)


  # Check global convergence of theta.d if not classifying
  global.conv <- check.conv(old.param.vec=post.old,
                                          new.param.vec=post.new,
                                          reltol=1e-6)

  out.list <- list(theta.d=theta.d.raw.new,global.conv=global.conv)

  return(out.list)
}






