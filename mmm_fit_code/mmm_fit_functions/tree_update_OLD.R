# Functions to update mu vector in mixed membership model
# and other tree parameters

#require(Matrix,quietly=TRUE)

# Function to get params and data in useful format (anything that
# doesn't involve tree parameters and should not be recalculated)
get.data.for.tree <- function(word.id,current.param.list,
                              doc.length.vec,doc.topic.list,
                              feature.count.list,topic.address.book){

  # Get full list of topics
  topics <- topic.address.book[,"topic"]

  # Grab need parameters from current.param.list
  theta.param.vecs <- current.param.list$theta.param.vecs
  K <- current.param.list$K
  psi <- current.param.list$psi
  gamma <- current.param.list$gamma
  nu <- current.param.list$nu
  sigma2 <- current.param.list$sigma2

  # Make sure have word id as a string and numeric
  word.id.str <- as.character(word.id)
  
  # Get doc-specific counts for this feature
  counts.feature <- feature.count.list[[word.id.str]]
  
  # Active docs for this word
  active.docs <- names(counts.feature)
  
  # theta.param.vecs is DxK
  # Save using sparse matrix representation since most
  # topic memberships are zero
  #X <- as(theta.param.vecs,"sparseMatrix")
  # Now sparsification done by master node
  X <- theta.param.vecs
  
  tree.data.list <- list(active.docs=active.docs,
                         counts.feature=counts.feature,
                         doc.length.vec=doc.length.vec,K=K,X=X,
                         psi=psi,gamma=gamma,nu=nu,sigma2=sigma2)

  return(tree.data.list)
}

# Function to get prior params for each mu
get.mu.prior.params <- function(topic,topic.address.book,mu.0.f,mu.f,tau2f.vec,
                                grad=FALSE,parent.child.list=NULL,
                                corpus.topic){
  
  # Make sure topic is a string
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
eval.mu.prior <- function(topic,topic.address.book,mu.0.f,
                          mu.f,tau2f.vec,corpus.topic){
  
  mu.prior.list <- get.mu.prior.params(topic=topic,
                                       topic.address.book=topic.address.book,
                                       corpus.topic=corpus.topic,
                                       mu.0.f=mu.0.f,mu.f=mu.f,
                                       tau2f.vec=tau2f.vec)
  mu.self <- mu.prior.list$mu.self
  mu.parent <- mu.prior.list$mu.parent
  tau2.parent <- mu.prior.list$tau2.parent
  mu.prior <- -0.5*log(tau2.parent)-(2*tau2.parent)^(-1)*(mu.self-mu.parent)^2
  return(mu.prior)
}

# Function to evaluate prior gradient for each mu.kf
eval.mu.prior.grad <- function(topic,topic.address.book,
                               mu.0.f,mu.f,tau2f.vec,
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

# Functions to evaluate prior and grad for corpus-level mu
eval.mu.0.prior <- function(mu.0.f,psi,gamma){
  mu.0.prior <- -(2*gamma^2)^(-1)*(mu.0.f-psi)^2
  return(mu.0.prior)
}

eval.mu.0.grad <- function(mu.0.f,tau2f.vec,mu.f,psi,gamma,
                           parent.child.list,corpus.topic){

  # Get parameters
  child.topics <- parent.child.list[[corpus.topic]]
  pos.children <- as.numeric(names(child.topics))
  mu.children <- mu.f[pos.children]
  tau2.0 <- tau2f.vec[corpus.topic]

  # Evaluate gradient
  mu.0.grad <- (tau2.0)^(-1)*sum(mu.children-mu.0.f) -
    (gamma^2)^(-1)*(mu.0.f-psi)
  return(mu.0.grad)
}


# Functions to evaluate prior and gradient for discrimination parameters

## # These versions for inverse chi-sq dist
## # Log prior for tau2f vector
## eval.tau2f.prior <- function(tau2f.vec,nu,sigma2){
##   tau2f.prior.sum <- -(1+0.5*nu)*sum(log(tau2f.vec)) -
##     0.5*nu*sigma2*sum((tau2f.vec)^(-1))
##   return(tau2f.prior.sum)
## }

## eval.tau2f.k.prior.grad <- function(tau2.self,nu,sigma2){
##   prior.grad <- -(1 + 0.5*nu)*(tau2.self)^(-1) + 0.5*nu*sigma2*(tau2.self)^(-2)
##   return(prior.grad)
## }


# Log prior for tau2f vector
eval.tau2f.prior <- function(tau2f.vec,nu,sigma2,dist="inv.chisq"){
  if (dist=="inv.chisq") {tau2f.prior.sum <- -(1+0.5*nu)*sum(log(tau2f.vec)) -
                            0.5*nu*sigma2*sum((tau2f.vec)^(-1))
  } else if (dist=="log.normal") {                      
    tau2f.prior.sum <- -sum(log(tau2f.vec)) - (2*sigma2)^(-1)*sum((log(tau2f.vec)-nu)^2)}
  return(tau2f.prior.sum)
}

eval.tau2f.k.prior.grad <- function(tau2.self,nu,sigma2,dist="inv.chisq"){
  if (dist=="inv.chisq") {prior.grad <- -(1 + 0.5*nu)*(tau2.self)^(-1) +
                            0.5*nu*sigma2*(tau2.self)^(-2)
  } else if (dist=="log.normal") {
    prior.grad <- -tau2.self^(-1)*(1 + (sigma2)^(-1)*(log(tau2.self)-nu))}
  return(prior.grad)
}

# Gradient for individual tau2f,k
eval.tau2f.k.grad <- function(n.child,tau2.self,mu.children,mu.self,nu,sigma2,dist="inv.chisq"){
  prior.grad <- eval.tau2f.k.prior.grad(tau2.self=tau2.self,nu=nu,sigma2=sigma2)
  grad <- -(n.child/2)*tau2.self^(-1) +
    0.5*sum((mu.children-mu.self)^2)*tau2.self^(-2) + prior.grad
  ## -(1 + 0.5*nu)*tau2.self^(-1) + 0.5*nu*sigma2*tau2.self^(-2)
  return(grad)
}

eval.tau2f.grad <- function(topic,tau2f.vec,mu.f,mu.0.f,nu,sigma2,
                            parent.child.list,corpus.topic,dist="inv.chisq"){
  # Get parameters
  child.topics <- parent.child.list[[topic]]
  pos.children <- as.numeric(names(child.topics))
  mu.children <- mu.f[pos.children]
  if (topic==corpus.topic) {mu.self <- mu.0.f
  } else {mu.self <- mu.f[topic]}
  tau2.self <- tau2f.vec[topic]
  n.child <- length(mu.children)

  # Evaluate gradient
  tau2f.k.grad <- eval.tau2f.k.grad(n.child=n.child,tau2.self=tau2.self,mu.children=mu.children,
                                    mu.self=mu.self,nu=nu,sigma2=sigma2,dist="inv.chisq")
  ## tau2f.k.grad <- -(n.child/2)*(tau2.self)^(-1) +
  ##   0.5*sum((mu.children-mu.self)^2)*(tau2.self)^(-2) -
  ##     (1 + 0.5*nu)*(tau2.self)^(-1) + 0.5*nu*sigma2*(tau2.self)^(-2)

  return(tau2f.k.grad)
}

# Function to evaluate log conditional posterior of tree parameters
# for feature f
tree.log.posterior <- function(par,tree.data.list,topic.address.book,
                               parent.child.list,corpus.topic,
                               dist.tau2="inv.chisq"){

  # Get vector of topics
  topics <- topic.address.book[,"topic"]
  
  # Unpack needed data
  active.docs <- tree.data.list$active.docs
  counts.feature <- tree.data.list$counts.feature
  X <- tree.data.list$X
  K <- tree.data.list$K
  psi <- tree.data.list$psi
  gamma <- tree.data.list$gamma
  nu <- tree.data.list$nu
  sigma2 <- tree.data.list$sigma2
  doc.length.vec <- tree.data.list$doc.length.vec

  # Extract parameters from par
  mu.f <- par[1:K]
  beta.f <- exp(mu.f)
  mu.0.f <- par[K+1]
  tau2f.vec <- exp(par[(K+2):length(par)])
  
  # Get linear predictor
  x.beta.vec <- as.vector(X%*%beta.f)
  l.x.beta.vec <- doc.length.vec*x.beta.vec
  names(x.beta.vec) <- names(l.x.beta.vec) <- names(doc.length.vec)

  # Get log prior of mu vector
  log.mu.prior.vec <- sapply(topics,eval.mu.prior,
                             topic.address.book=topic.address.book,
                             corpus.topic=corpus.topic,
                             mu.0.f=mu.0.f,mu.f=mu.f,
                             tau2f.vec=tau2f.vec)

  # Get log prior of mu.0.f
  log.mu.0.prior <- eval.mu.0.prior(mu.0.f=mu.0.f,psi=psi,gamma=gamma)

  # Get log prior for discrimination parameters
  log.tau2f.prior.sum <- eval.tau2f.prior(tau2f.vec=tau2f.vec,nu=nu,sigma2=sigma2,
                                          dist=dist.tau2)

  # Evaluate log posterior
  log.posterior <- -sum(l.x.beta.vec) +
    sum(counts.feature*log(x.beta.vec[active.docs])) +
      sum(log.mu.prior.vec) + log.mu.0.prior + log.tau2f.prior.sum

  #if(is.na(log.posterior)){browser()}
  
  return(as.numeric(log.posterior))
}


# Function to evaluate gradient of log conditional posterior of mu.f
tree.log.post.gradient <- function(par,tree.data.list,topic.address.book,
                                   parent.child.list,corpus.topic,
                                   dist.tau2="inv.chisq"){

  # Get vector of topics
  topics <- topic.address.book[,"topic"]
  
  # Unpack needed data
  active.docs <- tree.data.list$active.docs
  counts.feature <- tree.data.list$counts.feature
  X <- tree.data.list$X
  K <- tree.data.list$K
  psi <- tree.data.list$psi
  gamma <- tree.data.list$gamma
  nu <- tree.data.list$nu
  sigma2 <- tree.data.list$sigma2
  doc.length.vec <- tree.data.list$doc.length.vec

  # Extract parameters from par
  mu.f <- par[1:K]
  beta.f <- exp(mu.f)
  mu.0.f <- par[K+1]
  tau2f.vec <- exp(par[(K+2):length(par)])
  
  # Get linear predictor
  x.beta.vec <- as.vector(X%*%beta.f)
  names(x.beta.vec) <- rownames(X)

  # Get count ratio for active docs
  count.ratio <- counts.feature/x.beta.vec[active.docs]

  # Get column sums of X
  l.X.col.sums <- colSums(doc.length.vec*X)

  # Get log prior gradient of mu vector
  log.mu.prior.grad <- sapply(topics,eval.mu.prior.grad,
                           topic.address.book=topic.address.book,
                           mu.0.f=mu.0.f,mu.f=mu.f,
                           tau2f.vec=tau2f.vec,
                           parent.child.list=parent.child.list,
                           corpus.topic=corpus.topic)

  # Get log prior gradient of corpus-level mu
  log.mu.0.prior.grad <- eval.mu.0.grad(mu.0.f=mu.0.f,
                                        tau2f.vec=tau2f.vec,
                                        mu.f=mu.f,psi=psi,gamma=gamma,
                                        parent.child.list=parent.child.list,
                                        corpus.topic=corpus.topic)

  # Get log prior gradient for tau2 vector
  parent.topics <- names(tau2f.vec)
  log.tau2.prior.grad <- sapply(parent.topics,eval.tau2f.grad,
                                tau2f.vec=tau2f.vec,
                                mu.f=mu.f,mu.0.f=mu.0.f,
                                nu=nu,sigma2=sigma2,
                                parent.child.list=parent.child.list,
                                corpus.topic=corpus.topic,
                                dist=dist.tau2)

  # Evaluate log posterior gradient
  ## # Need to make exception for words active in only one doc
  ## if(length(active.docs)==1){X.active <- matrix(X[active.docs,],nrow=1)}
  ## else{X.active <- X[active.docs,]}
  X.active <- X[active.docs,,drop=FALSE]
  gradient.likelihood <- -l.X.col.sums + as.vector(count.ratio%*%X.active)
  # Convert gradient wrt beta to mu space and add prior grad
  gradient.mu <- gradient.likelihood*beta.f + log.mu.prior.grad

  # Get gradient for entire tree of parameters
  # Include chain rule for discrim params since guessing in log space
  gradient.tree <- c(gradient.mu,log.mu.0.prior.grad,
                     log.tau2.prior.grad*tau2f.vec)
  names(gradient.tree) <- names(par)
  
  return(gradient.tree)
}


optim.tree <- function(job.id,current.param.list,doc.length.vec,
                       doc.topic.list,feature.count.list,topic.address.book,
                       corpus.topic="CORPUS",hessian=FALSE,tree.data.out=FALSE,
                       dist.tau2="inv.chisq"){

  # Get old parameter values from last update
  mu.f.old <- current.param.list$mu.param.vecs[job.id,]
  mu.0.f.old <- current.param.list$mu.corpus.vec[job.id]
  tau2f.vec.old <- current.param.list$tau2.param.vecs[job.id,]
  old.param.vec <- c(mu.f.old,mu.0.f.old,tau2f.vec.old)
  
  # Start optimizer at old parameter values
  par <- c(mu.f.old,mu.0.f.old,log(tau2f.vec.old))
  #par <- old.param.vec
  
  tree.data.list <- get.data.for.tree(word.id=job.id,
                                      current.param.list=current.param.list,
                                      doc.length.vec=doc.length.vec,
                                      doc.topic.list=doc.topic.list,
                                      feature.count.list=feature.count.list,
                                      topic.address.book=topic.address.book)

  ## # Get number of topics (and mu.f parameters)
  ## K <- tree.data.list$K

  # Get starting value of posterior
  post.old <- tree.log.posterior(par=par,tree.data.list=tree.data.list,
                                 topic.address.book=topic.address.book,
                                 parent.child.list=
                                 current.param.list$parent.child.list,
                                 corpus.topic=corpus.topic)

  optim.out.bfgs <- optim(par=par,fn=tree.log.posterior,
                          gr=tree.log.post.gradient,
                          control=list(fnscale=-1),
                          tree.data.list=tree.data.list,
                          topic.address.book=topic.address.book,
                          parent.child.list=
                          current.param.list$parent.child.list,
                          corpus.topic=corpus.topic,
                          method="L-BFGS-B",
                          # Need lower bound on params to ensure stability
                          # of convergence check
                          # Note that optim guessing everything in log space
                          lower=0.5*log(.Machine$double.eps),
                          hessian=hessian,
                          dist.tau2=dist.tau2)
  
  optim.bfgs.par <- optim.out.bfgs$par
  K <- tree.data.list$K
  mu.f.new <- optim.bfgs.par[1:K]
  mu.0.f.new <- optim.bfgs.par[K+1]
  tau2f.vec.new <- exp(optim.bfgs.par[(K+2):length(optim.bfgs.par)])
  #tau2f.vec.new <- optim.bfgs.par[(K+2):length(optim.bfgs.par)]
  new.param.vec <- c(mu.f.new,mu.0.f.new,tau2f.vec.new)

  if(hessian){hessian.tree <- optim.out.bfgs$hessian}

  # Get new value of posterior
  post.new <- tree.log.posterior(par=optim.bfgs.par,
                                 tree.data.list=tree.data.list,
                                 topic.address.book=topic.address.book,
                                 parent.child.list=
                                 current.param.list$parent.child.list,
                                 corpus.topic=corpus.topic)

  #print(c(post.old,post.new))

  # Check global convergence of tree parameters
  global.conv <- check.conv(old.param.vec=post.old,
                            new.param.vec=post.new,
                            reltol=1e-6)
  
  out.list <- list(mu.f=mu.f.new,mu.0.f=mu.0.f.new,
                   tau2f.vec=tau2f.vec.new,global.conv=global.conv,
                   tree.post=post.new,tree.post.old=post.old)
  if(hessian){out.list$hessian.tree <- hessian.tree}
  if(tree.data.out){out.list$tree.data.list <- tree.data.list}
  return(out.list)
}
