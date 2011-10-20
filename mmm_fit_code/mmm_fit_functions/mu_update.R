# Functions to update mu vector in mixed membership model
# and other tree parameters

library(Matrix,quietly=TRUE)
source("check.conv.R")

# Function to get params and data in useful format (anything that
# doesn't involve mu and should not be recalculated)
get.data.for.mu <- function(word.id,theta.param.vecs,
                            mu.corpus.vec,tau2.param.vecs,
                            doc.length.vec,
                            doc.topic.list,count.list,topic.address.book){

  # Get full list of topics
  topics <- topic.address.book[,"topic"]

  # Make sure have word id as a string and numeric
  word.id.str <- toString(word.id)
  #word.id <- as.numeric(word.id)
  
  # Get doc-specific counts for this feature
  counts.feature <- count.list[[word.id.str]]

  # Get params for this feature
  tau2f.vec <- tau2.param.vecs[word.id.str,]
  mu.0.f <- mu.corpus.vec[word.id.str]
  
  # Active docs for this word
  active.docs <- as.numeric(names(counts.feature))
  
  # theta.param.vecs is DxK
  # Save using sparse matrix representation since most
  # topic memberships are zero
  X <- as(theta.param.vecs,"sparseMatrix")
  #X <- theta.param.vecs

  mu.data.list <- list(active.docs=active.docs,
                       counts.feature=counts.feature,
                       doc.length.vec=doc.length.vec,
                       X=X,tau2f.vec=tau2f.vec,mu.0.f=mu.0.f)

  return(mu.data.list)
}

# Function to get prior params for each mu
get.mu.prior.params <- function(topic,topic.address.book,mu.0.f,mu.f,tau2f.vec,
                                grad=FALSE,parent.child.list=NULL,
                                corpus.topic){
  
  # Weird hack to make sure topic is a string
  topic <- toString(topic)
  
  # Figure out parent topic
  pos.topic.address.book <- which(topic.address.book[,"topic"]==topic)
  parent <- topic.address.book[pos.topic.address.book,"parent"]
  
  # Have to treat mus at highest level differently
  if(parent==corpus.topic){
    mu.parent <- mu.0.f
    tau2.parent <- tau2f.vec[corpus.topic]
  }
  
  # Mus below highest level
  else{
    mu.parent <- mu.f[parent]
    tau2.parent <- tau2f.vec[parent]
  }

  # Get child mus if this is for the gradient
  if(grad){
    if(is.null(parent.child.list)){
      stop("Must supply parent.child.list to get params for gradient")}
    child.topics <- parent.child.list[[topic]]
    # Terminal nodes
    is.terminal <- is.null(child.topics)
    if(is.terminal){mu.children <- NULL}
    # Non-terminal nodes
    else{pos.children <- as.numeric(names(child.topics))
         mu.children <- mu.f[pos.children]
         tau2.self <- tau2f.vec[topic]}
  }
  
  mu.self <- mu.f[topic]
  mu.prior.list <- list(mu.self=mu.self,mu.parent=mu.parent,
                        tau2.parent=tau2.parent)
  # Add extra items to output list if gradient calculation
  if(grad){mu.prior.list[["mu.children"]] <- mu.children
           if(!is.terminal){mu.prior.list[["tau2.self"]] <- tau2.self}}
  return(mu.prior.list)
}

# Function to evaluate prior for each mu.kf
eval.mu.prior <- function(topic,topic.address.book,mu.0.f,mu.f,tau2f.vec,
                          corpus.topic){
  mu.prior.list <- get.mu.prior.params(topic=topic,
                                       topic.address.book=topic.address.book,
                                       corpus.topic=corpus.topic,
                                       mu.0.f=mu.0.f,mu.f=mu.f,
                                       tau2f.vec=tau2f.vec)
  mu.self <- mu.prior.list$mu.self
  mu.parent <- mu.prior.list$mu.parent
  tau2.parent <- mu.prior.list$tau2.parent
  mu.prior <- -(2*tau2.parent)^(-1)*(mu.self-mu.parent)^2
  return(mu.prior)
}

# Function to evaluate prior gradient for each mu.kf
eval.mu.prior.grad <- function(topic,topic.address.book,mu.0.f,mu.f,tau2f.vec,
                               parent.child.list,corpus.topic){
  mu.prior.list <- get.mu.prior.params(topic=topic,
                                       topic.address.book=topic.address.book,
                                       mu.0.f=mu.0.f,mu.f=mu.f,
                                       tau2f.vec=tau2f.vec,grad=TRUE,
                                       parent.child.list=parent.child.list,
                                       corpus.topic=corpus.topic)
  mu.self <- mu.prior.list$mu.self
  mu.parent <- mu.prior.list$mu.parent
  tau2.parent <- mu.prior.list$tau2.parent
  tau2.self <- mu.prior.list$tau2.self
  mu.children <- mu.prior.list$mu.children
  mu.prior.grad.self <- -tau2.parent^(-1)*(mu.self-mu.parent)
  # Have to deal with terminal nodes separately
  if(is.null(mu.children)){mu.prior.grad <- mu.prior.grad.self}
  else{mu.prior.grad.child <- sum(tau2.self^(-1)*(mu.children-mu.self))
       mu.prior.grad <- mu.prior.grad.self + mu.prior.grad.child}
  return(mu.prior.grad)
}

# Function to evaluate log conditional posterior of mu.f
mu.log.posterior <- function(par,mu.data.list,topic.address.book,
                             parent.child.list,corpus.topic){

  # Create beta and vectors
  mu.f <- par
  beta.f <- exp(par)
  #print(mu.f)
  #print(mu.data.list$mu.0.f)
  #print("")

  # Get vector of topics
  topics <- topic.address.book[,"topic"]
  
  # Unpack needed data
  active.docs <- mu.data.list$active.docs
  counts.feature <- mu.data.list$counts.feature
  X <- mu.data.list$X
  tau2f.vec <- mu.data.list$tau2f.vec
  mu.0.f <- mu.data.list$mu.0.f
  doc.length.vec <- mu.data.list$doc.length.vec
  
  # Get linear predictor
  l.x.beta.vec <- as.vector(doc.length.vec*X%*%beta.f)
  x.beta.vec <- as.vector(X%*%beta.f)

  # Get log prior of mu vector
  log.prior.vec <- sapply(topics,eval.mu.prior,
                          topic.address.book=topic.address.book,
                          corpus.topic=corpus.topic,
                          mu.0.f=mu.0.f,mu.f=mu.f,
                          tau2f.vec=tau2f.vec)

  # Evaluate log posterior
  log.posterior <- -sum(l.x.beta.vec)+
    sum(counts.feature*log(x.beta.vec[active.docs]))+
      sum(log.prior.vec)

  #print(paste("p1",-sum(l.x.beta.vec)))
  #print(paste("p2",sum(counts.feature*log(x.beta.vec[active.docs]))))
  #print(mu.0.f)
  #print(tau2f.vec)
  #print(log.posterior)
  
  return(log.posterior)
}


# Function to evaluate gradient of log conditional posterior of mu.f
mu.log.post.gradient <- function(par,mu.data.list,topic.address.book,
                                 parent.child.list,corpus.topic){

  # Create beta and vectors
  mu.f <- par
  beta.f <- exp(par)

  # Get vector of topics
  topics <- topic.address.book[,"topic"]
  
  # Unpack needed data
  active.docs <- mu.data.list$active.docs
  counts.feature <- mu.data.list$counts.feature
  X <- mu.data.list$X
  tau2f.vec <- mu.data.list$tau2f.vec
  mu.0.f <- mu.data.list$mu.0.f
  doc.length.vec <- mu.data.list$doc.length.vec
  
  # Get linear predictor
  x.beta.vec <- as.vector(X%*%beta.f)

  # Get count ratio for active words
  count.ratio <- counts.feature/x.beta.vec[active.docs]

  # Get column sums of X
  l.X.col.sums <- colSums(doc.length.vec*X)

  # Get log prior gradient of mu vector
  log.prior.grad <- sapply(topics,eval.mu.prior.grad,
                           topic.address.book=topic.address.book,
                           mu.0.f=mu.0.f,mu.f=mu.f,
                           tau2f.vec=tau2f.vec,
                           parent.child.list=parent.child.list,
                           corpus.topic=corpus.topic)

  # Evaluate log posterior gradient
  # Need to make exception for words active in only one doc
  if(length(active.docs)==1){X.active <- matrix(X[active.docs,],nrow=1)}
  else{X.active <- X[active.docs,]}
  gradient.likelihood <- -l.X.col.sums + as.vector(count.ratio%*%X.active)
  # Convert gradient wrt beta to mu space and add prior grad
  gradient.mu <- gradient.likelihood*beta.f + log.prior.grad
  
  return(gradient.mu)
}


# Coordinate ascent algorithm for tree parameters
coor.ascent.tree <- function(job.id,current.param.list,doc.length.vec,
                             doc.topic.list,feature.count.list,
                             topic.address.book,
                             corpus.topic="CORPUS",
                             max.iter=500,
                             reltol=sqrt(.Machine$double.eps),
                             verbose=FALSE){

  mu.f.vec.new <-current.param.list$mu.param.vecs[job.id,]
  #noise <- rnorm(mean=0,n=length(mu.f.vec.new))
  #mu.f.vec.new <- mu.f.vec.new + noise
  parent.child.list <- current.param.list$parent.child.list
  theta.param.vecs <- current.param.list$theta.param.vecs
  mu.corpus.vec <- current.param.list$mu.corpus.vec
  tau2.param.vecs <- current.param.list$tau2.param.vecs
  
  mu.data.list <- get.data.for.mu(word.id=job.id,
                                  theta.param.vecs=theta.param.vecs,
                                  mu.corpus.vec=mu.corpus.vec,
                                  tau2.param.vecs=tau2.param.vecs,
                                  doc.length.vec=doc.length.vec,
                                  doc.topic.list=doc.topic.list,
                                  count.list=feature.count.list,
                                  topic.address.book=topic.address.book)

  # Put together initial parameters so can check global convergence
  tau2f.vec <- mu.data.list$tau2f.vec
  mu.0.f <- mu.data.list$mu.0.f
  vec.orig <- c(mu.f.vec.new,mu.0.f,tau2f.vec)

  # Iterate updates until convergence
  for(i in 1:max.iter){
    # Replace old set of parameters
    if(i==1){vec.old <- vec.orig}
    if(i>1){vec.old <- vec.out}

    optim.out.bfgs <- optim(par=mu.f.vec.new,fn=mu.log.posterior,
                            gr=mu.log.post.gradient,
                            control=list(fnscale=-1),
                            mu.data.list=mu.data.list,
                            topic.address.book=topic.address.book,
                            parent.child.list=parent.child.list,
                            corpus.topic=corpus.topic,
                            method="L-BFGS-B")
    mu.f.vec.new <- optim.out.bfgs$par
    
    hparam.out <- update.tree.hparams(mu.f=mu.f.vec.new,
                                      current.param.list=current.param.list,
                                      mu.data.list=mu.data.list,
                                      parent.child.list=parent.child.list,
                                      corpus.topic=corpus.topic,
                                      topic.address.book=topic.address.book)
    mu.0.f.new <- hparam.out$mu.0.f
    tau2f.vec.new <- hparam.out$tau2f.vec
    mu.data.list$mu.0.f <- mu.0.f.new
    mu.data.list$tau2f.vec <- tau2f.vec.new

    vec.out <- c(mu.f.vec.new,mu.0.f.new,tau2f.vec.new)
    #print(list(mu.f.vec.new,mu.0.f.new,tau2f.vec.new))
    
    # Check convergence
    conv <- check.conv(old.param.vec=vec.old,new.param.vec=vec.out,
                       reltol=reltol)
    if(conv==TRUE){break}
  }

  # Print convergence info if requested
  if(verbose){
    if(i<max.iter){print(paste("Converged in", i, "iterations"))}
    if(i==max.iter){
      print(paste("Failed to converge within", max.iter, "iterations"))}
  }

  # Check for global convergence of parameters
  global.conv <- check.conv(old.param.vec=vec.orig,new.param.vec=vec.out,
                            reltol=reltol)
  
  param.new.list <- list(mu.f.vec=mu.f.vec.new,mu.0.f=mu.0.f.new,
                         tau2f.vec=tau2f.vec.new,global.conv=global.conv)
  return(param.new.list)
}
