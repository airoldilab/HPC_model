# Functions to update mu vector in mixed membership model
# and other tree parameters

require("Matrix",quietly=TRUE)

# Function to get params and data in useful format (anything that
# doesn't involve tree parameters and should not be recalculated)
# Also including case-control sampling functionality so that
# only calculate likelihood for a subset of documents
# See Raftery et al. (2011)
get.data.for.tree <- function(word.id,current.param.list,
                              doc.length.vec,doc.topic.list,
                              feature.count.list,topic.address.book,
                              get.prior.hes=FALSE,
                              Ndoc.case.control=NULL){

  # Get full list of topics
  topics <- topic.address.book[,"topic"]

  # Grab need parameters from current.param.list
  K <- current.param.list$K
  psi <- current.param.list$psi
  gamma <- current.param.list$gamma
  nu <- current.param.list$nu
  sigma2 <- current.param.list$sigma2
  tau2f.vec <- current.param.list$tau2.param.vecs[word.id,]
  
  # Make sure have word id as a string and numeric
  word.id.str <- as.character(word.id)
  
  # Get doc-specific counts for this feature
  counts.feature <- feature.count.list[[word.id.str]]
  
  # Active docs for this word
  active.docs <- names(counts.feature)
  N.active <- length(active.docs)
  N.doc <- current.param.list$D
  
  # theta.param.vecs is DxK
  X <- current.param.list$theta.param.vecs
  X.active <- X[active.docs,,drop=FALSE]

  # Do case control sampling of covariate matrix if requested
  if(!is.null(Ndoc.case.control)){
   out.case.cont <- case.control.samp(X=X,X.active=X.active,
                                      N.samp=Ndoc.case.control,
                                      active.index=active.docs,
                                      doc.length.vec=doc.length.vec)
   index.samp <- out.case.cont$index.samp
   active.samp <- out.case.cont$active.samp
   X <- out.case.cont$X
   X.active <- out.case.cont$X.active
   l.X.col.sums <- out.case.cont$X.col.sums
   like.mult <- out.case.cont$like.mult
   case.control.correct <- out.case.cont$case.control.correct
   # Make sure that other data objects have only sampled units and
   # in correct order
   doc.length.vec <- out.case.cont$doc.length.vec
   counts.feature <- counts.feature[active.samp]
   active.docs <- active.samp
   
  } else {
    # Cache column sums of X for gradient calculation without weights
    l.X.col.sums <- colSums(doc.length.vec*X)
    case.control.correct <- FALSE
    like.mult <- NULL
  }

  # Put together output list
  tree.data.list <- list(active.docs=active.docs,
                         counts.feature=counts.feature,
                         doc.length.vec=doc.length.vec,K=K,X=X,
                         X.active=X.active,l.X.col.sums=l.X.col.sums,
                         psi=psi,gamma=gamma,nu=nu,sigma2=sigma2,
                         tau2f.vec=tau2f.vec,like.mult=like.mult,
                         case.control.correct=case.control.correct)

  # Save weight information if doing case control sampling and
  # need correction
  if(case.control.correct){
    tree.data.list$weight.active.samp <- out.case.cont$weight.active.samp
    tree.data.list$weight.inactive.samp <- out.case.cont$weight.inactive.samp
    tree.data.list$inactive.samp <- out.case.cont$inactive.samp
  }
  
  # Get key pieces of analytic Hessian if requested
  if(get.prior.hes){
    parent.child.list <- current.param.list$parent.child.list
    hes.prior <- get.hessian.prior(tau2.vec=tau2f.vec,
                                   topic.address.book=topic.address.book,
                                   parent.child.list=parent.child.list,
                                   gamma=gamma,sparse=FALSE)
    tree.data.list$hes.chol <- chol(-hes.prior)
    tree.data.list$hes.prior <- hes.prior
  }
  
  return(tree.data.list)
}


# Function to get hessian of prior---note that this has one extra
# dimension from hessian of likelihood
get.hessian.prior <- function(tau2.vec,topic.address.book,
                              parent.child.list,gamma,
                              corpus.topic="CORPUS",sparse=TRUE){
  topics <- c(topic.address.book[,"topic"],corpus.topic)
  ntopics <- length(topics)
  inv.tau2.vec <- 1/tau2.vec
  # Make hes.prior a sparse matrix if requested
  if(sparse){
    hes.prior <- sparseMatrix(i=NULL,j=NULL,x=0,
                              dims=c(ntopics,ntopics),
                              dimnames=list(topics,topics))
  } else {
    hes.prior <- matrix(0,length(topics),length(topics))
    rownames(hes.prior) <- colnames(hes.prior) <- topics
  }

  for (k in 1:(ntopics-1)) {
    # Figure out parent topic
    #pos.topic.address.book <- which(topic.address.book[,"topic"]==topic)
    #parent <- topic.address.book[pos.topic.address.book,"parent"]
    topic <- topic.address.book[k,"topic"]
    parent <- topic.address.book[k,"parent"]
    hes.prior[topic,parent] <- inv.tau2.vec[parent]

    # Figure out child topics
    child.topics <- parent.child.list[[topic]]
    is.terminal <- is.null(child.topics)
    if(!is.terminal){
      # Non-terminal nodes
      hes.prior[topic,child.topics] <- inv.tau2.vec[topic]
      nchild <- length(child.topics)
      hes.prior[topic,topic] <- -nchild*inv.tau2.vec[topic]-inv.tau2.vec[parent]
      
    } else {
      # Terminal nodes
      hes.prior[topic,topic] <- -inv.tau2.vec[parent]
      #hes.prior[topic,parent] <- inv.tau2.vec[parent]
    }
  }
  
  # Finally, deal with corpus level "topic"
  child.topics <- parent.child.list[[corpus.topic]]
  nchild <- length(child.topics)
  hes.prior[corpus.topic,corpus.topic] <- -1/gamma^2 -
    nchild*inv.tau2.vec[corpus.topic]
  hes.prior[corpus.topic,child.topics] <- inv.tau2.vec[corpus.topic]

  return(hes.prior)
}


# Function to evaluate hessian of rate likelihood
get.hessian.like <- function(par,tree.data.list,n.sample=NULL,
                             diag.only=FALSE){

  # Unpack needed data
  active.docs <- tree.data.list$active.docs
  counts.feature <- tree.data.list$counts.feature
  X <- tree.data.list$X
  K <- tree.data.list$K
  psi <- tree.data.list$psi
  gamma <- tree.data.list$gamma
  doc.length.vec <- tree.data.list$doc.length.vec

  # Extract parameters from par
  mu.f <- par[1:K]
  beta.f <- exp(mu.f)
  mu.0.f <- par[K+1]

  # Get gradient at these parameter values
  grad.like <- eval.like.grad(mu.f=mu.f,tree.data.list=tree.data.list)
  
  # Get linear predictor
  x.beta.vec <- as.vector(X%*%beta.f)
  names(x.beta.vec) <- rownames(X)

  # Get count ratio for active docs
  count.ratio <- counts.feature/(x.beta.vec[active.docs])^2

  # Figure out which documents to use for calculating Hessian
  n.active <- length(active.docs)
  if(is.null(n.sample)){docs.use <- 1:n.active
  } else {
    if(n.sample>=n.active){docs.use <- 1:n.active
    } else {
      docs.use <- sample.int(n=n.active,size=n.sample)
    }
  }
  
  X.active <- X[active.docs,,drop=FALSE]

  # Fast procedure for only calculating diagonal of Hessian
  if(diag.only){
    hessian.beta.diag <- colSums(count.ratio[docs.use]*(X.active[docs.use,]^2))
    
    # Scale up hessian.beta if necessary
    if(!is.null(n.sample)){hessian.beta.diag <- (n.active/n.sample)*hessian.beta.diag}

    # Scale up hessian if case control sampling used
    if(tree.data.list$case.control.correct){
      hessian.beta.diag <- tree.data.list$weight.active.samp*hessian.beta.diag
    } else if(!is.null(tree.data.list$like.mult)){
      hessian.beta.diag <- tree.data.list$like.mult*hessian.beta.diag
    }

    # Use chain rule to get back to log space
    hessian.mu.diag <- -hessian.beta.diag*beta.f^2 + grad.like
    hessian.mu.like <- hessian.mu.diag
    
  # Otherwise calculate entire Hessian
  } else {
    X.active.cr <- sqrt(count.ratio[docs.use])*X.active[docs.use,]
    ## hessian.beta <- t(X.active.cr)%*%X.active.cr
    hessian.beta <- crossprod(X.active.cr)

    # Scale up hessian if case control sampling used
    if(tree.data.list$case.control.correct){
      hessian.beta <- tree.data.list$weight.active.samp*hessian.beta
    } else if(!is.null(tree.data.list$like.mult)){
      hessian.beta <- tree.data.list$like.mult*hessian.beta
    }
    
    # Scale up hessian.beta if subsampling done in this function
    if(!is.null(n.sample)){hessian.beta <- (n.active/n.sample)*hessian.beta}
    
    # Use chain rule to get back to log space
    hessian.mu.like <- as.matrix(-hessian.beta)*(beta.f%*%t(beta.f))
    diag(hessian.mu.like) <- diag(hessian.mu.like) + grad.like
  }

  return(hessian.mu.like)
}


# Function to evaluate hessian of rate posterior
eval.mu.hessian <- function(par,tree.data.list,n.sample=NULL,
                            hessian.like=NULL,diag.only=FALSE){

  K <- tree.data.list$K
  hes.prior <- tree.data.list$hes.prior

  if(is.null(hessian.like)){
    # Get hessian of likelihood in mu space
    hessian.like <- get.hessian.like(par=par,tree.data.list=tree.data.list,
                                     n.sample=n.sample,diag.only=diag.only)
  }

  if(diag.only){
    hes.prior <- c(diag(hes.prior)[1:K] + hessian.like, diag(hes.prior)[K+1])
  } else {hes.prior[1:K,1:K] <- hes.prior[1:K,1:K] + hessian.like}
  
  return(hes.prior)
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
  } else {
    # Mus below highest level
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
    if(is.terminal){mu.children <- NULL
    } else {
    # Non-terminal nodes
    pos.children <- as.numeric(names(child.topics))
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
  #mu.prior <- -0.5*log(tau2.parent)-(2*tau2.parent)^(-1)*(mu.self-mu.parent)^2
  mu.prior <- -(2*tau2.parent)^(-1)*(mu.self-mu.parent)^2
  return(mu.prior)
}

# Function to evaluate prior gradient of all mu quickly given prior hessian
# NOTE: This only works when max.tau2=FALSE
eval.mu.prior.grad <- function(par,psi,hes.prior){
  mu.prior.grad <- as.vector(hes.prior%*%(par-psi))
  return(mu.prior.grad)
}



# Function to evaluate prior gradient for each mu.kf
eval.mu.k.prior.grad <- function(topic,topic.address.book,
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
  tau2f.k.grad <- eval.tau2f.k.grad(n.child=n.child,tau2.self=tau2.self,
                                    mu.children=mu.children,
                                    mu.self=mu.self,nu=nu,sigma2=sigma2,
                                    dist="inv.chisq")

  return(tau2f.k.grad)
}


# Function to evaluate likelihood of data given tree parameters
tree.log.like <- function(mu.f,tree.data.list){

  # Get parameter vec of likelihood
  beta.f <- exp(mu.f)

  # Unpack needed data
  active.docs <- tree.data.list$active.docs
  counts.feature <- tree.data.list$counts.feature
  X <- tree.data.list$X
  K <- tree.data.list$K
  doc.length.vec <- tree.data.list$doc.length.vec

  # Get linear predictor
  x.beta.vec <- as.vector(X%*%beta.f)
  names(x.beta.vec) <- names(doc.length.vec)
  l.x.beta.vec <- doc.length.vec*x.beta.vec

  # Evaluate log likelihood
  # Use weighted sum if doing case control sampling
  if(tree.data.list$case.control.correct){
    weight.active.samp <- tree.data.list$weight.active.samp
    weight.inactive.samp <- tree.data.list$weight.inactive.samp
    inactive.samp <- tree.data.list$inactive.samp
    log.like <- -weight.inactive.samp*sum(l.x.beta.vec[inactive.samp]) -
      weight.active.samp*sum(l.x.beta.vec[active.docs]) +
            weight.active.samp*sum(counts.feature*log(x.beta.vec[active.docs]))
    
  # Else just get unweighted sum of likelihood terms
  } else {log.like <- -sum(l.x.beta.vec) +
            sum(counts.feature*log(x.beta.vec[active.docs]))}

  # If need to scale likelihood by constant, do this now
  if(!is.null(tree.data.list$like.mult)){
    log.like <- log.like*tree.data.list$like.mult}

  return(log.like)
}


# Function to evaluate log conditional posterior of tree parameters
# for feature f
tree.log.posterior <- function(par,tree.data.list,topic.address.book,
                               parent.child.list,corpus.topic,
                               max.tau2=TRUE,dist.tau2="inv.chisq",
                               print.post.error=FALSE){
  
  # Unpack needed data
  ## active.docs <- tree.data.list$active.docs
  ## counts.feature <- tree.data.list$counts.feature
  ## X <- tree.data.list$X
  K <- tree.data.list$K
  psi <- tree.data.list$psi
  gamma <- tree.data.list$gamma
  nu <- tree.data.list$nu
  sigma2 <- tree.data.list$sigma2
  ## doc.length.vec <- tree.data.list$doc.length.vec
  
  # Extract parameters from par
  mu.f <- par[1:K]
  beta.f <- exp(mu.f)
  mu.0.f <- par[K+1]
  if(max.tau2){ tau2f.vec <- exp(par[(K+2):length(par)])
  } else { tau2f.vec <- tree.data.list$tau2f.vec }

  # Get likelihood of data
  log.like <- tree.log.like(mu.f=mu.f,tree.data.list=tree.data.list)

  # Evaluate tree prior of mus (and tau2s if maxing over them)
  if(max.tau2){
    # Get vector of topics
    topics <- topic.address.book[,"topic"]
  
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

    mu.prior.sum <- sum(log.mu.prior.vec) + log.mu.0.prior
    
  } else {
    deviat.vec <- c(mu.f,mu.0.f)-psi
    ## Sigma.inv <- -tree.data.list$hes.prior
    ## mu.prior.sum <- -0.5*as.numeric(deviat.vec%*%Sigma.inv%*%deviat.vec)
    ss.sqrt <- tree.data.list$hes.chol%*%deviat.vec
    mu.prior.sum <- -0.5*sum(ss.sqrt^2)
  }
  
  # Evaluate log posterior
  log.posterior <- log.like + mu.prior.sum + ifelse(max.tau2,log.tau2f.prior.sum,0)

  if(print.post.error){
    if(any(is.na(log.posterior),is.nan(log.posterior))){
      print(list(log.like=log.like,mu.prior.sum=mu.prior.sum,mu.f=mu.f,mu.0.f=mu.0.f,
                 tau2f.vec=tree.data.list$tau2f.vec,psi=psi,gamma=gamma,nu=nu,sigma2=sigma2))
    }}
  
  return(as.numeric(log.posterior))
}


# Function to evaluate gradient of likelihood
eval.like.grad <- function(mu.f,tree.data.list){

  # Need needed data/parameter values
  beta.f <- exp(mu.f)
  X <- tree.data.list$X
  X.active <- tree.data.list$X.active
  active.docs <- tree.data.list$active.docs
  counts.feature <- tree.data.list$counts.feature
  doc.length.vec <- tree.data.list$doc.length.vec
  l.X.col.sums <- tree.data.list$l.X.col.sums

  # Evaluate log posterior gradient
  x.beta.active <- as.vector(X.active%*%beta.f)
  count.ratio <- counts.feature/x.beta.active

  # Use weighted sum if doing case control sampling
  if(tree.data.list$case.control.correct){
    weight.active.samp <- tree.data.list$weight.active.samp
    weight.inactive.samp <- tree.data.list$weight.inactive.samp
    gradient.likelihood <- -l.X.col.sums +
      weight.active.samp*as.vector(count.ratio%*%X.active)
  } else {
    gradient.likelihood <- -l.X.col.sums + as.vector(count.ratio%*%X.active)}

  # If need to scale grad by constant, do this now
  if(!is.null(tree.data.list$like.mult)){
    gradient.likelihood <- gradient.likelihood*tree.data.list$like.mult}
  
  # Convert gradient wrt beta to mu space
  gradient.mu <- gradient.likelihood*beta.f

  return(gradient.mu)
}


# Function to evaluate gradient of log conditional posterior of mu.f
tree.log.post.gradient <- function(par,tree.data.list,topic.address.book,
                                   parent.child.list,corpus.topic,
                                   max.tau2=TRUE,dist.tau2="inv.chisq",
                                   print.post.error=FALSE){

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
  hes.prior <- tree.data.list$hes.prior

  # Extract parameters from par
  mu.f <- par[1:K]
  beta.f <- exp(mu.f)
  mu.0.f <- par[K+1]
  if(max.tau2){ tau2f.vec <- exp(par[(K+2):length(par)])
  } else { tau2f.vec <- tree.data.list$tau2f.vec }
  
  ## # Get linear predictor
  ## x.beta.vec <- as.vector(X%*%beta.f)
  ## names(x.beta.vec) <- rownames(X)

  ## # Get count ratio for active docs
  ## count.ratio <- counts.feature/x.beta.vec[active.docs]

  ## # Get column sums of X
  ## l.X.col.sums <- colSums(doc.length.vec*X)

  ## # Get log prior gradient of mu vector
  ## log.mu.prior.grad <- sapply(topics,eval.mu.k.prior.grad,
  ##                             topic.address.book=topic.address.book,
  ##                             mu.0.f=mu.0.f,mu.f=mu.f,
  ##                             tau2f.vec=tau2f.vec,
  ##                             parent.child.list=parent.child.list,
  ##                             corpus.topic=corpus.topic)

  ## # Get log prior gradient of corpus-level mu
  ## log.mu.0.prior.grad <- eval.mu.0.grad(mu.0.f=mu.0.f,
  ##                                       tau2f.vec=tau2f.vec,
  ##                                       mu.f=mu.f,psi=psi,gamma=gamma,
  ##                                       parent.child.list=parent.child.list,
  ##                                       corpus.topic=corpus.topic)

  if(max.tau2){
    # Get log prior gradient for tau2 vector
    parent.topics <- names(tau2f.vec)
    log.tau2.prior.grad <- sapply(parent.topics,eval.tau2f.grad,
                                  tau2f.vec=tau2f.vec,
                                  mu.f=mu.f,mu.0.f=mu.0.f,
                                  nu=nu,sigma2=sigma2,
                                  parent.child.list=parent.child.list,
                                  corpus.topic=corpus.topic,
                                  dist=dist.tau2)

    # Get log prior gradient of mu vector
    log.mu.prior.grad <- sapply(topics,eval.mu.k.prior.grad,
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
    
  } else {
    ## # Get log prior gradient of mu vector
    ## log.mu.prior.grad <- sapply(topics,eval.mu.k.prior.grad,
    ##                             topic.address.book=topic.address.book,
    ##                             mu.0.f=mu.0.f,mu.f=mu.f,
    ##                             tau2f.vec=tau2f.vec,
    ##                             parent.child.list=parent.child.list,
    ##                             corpus.topic=corpus.topic)

    ## # Get log prior gradient of corpus-level mu
    ## log.mu.0.prior.grad <- eval.mu.0.grad(mu.0.f=mu.0.f,
    ##                                       tau2f.vec=tau2f.vec,
    ##                                       mu.f=mu.f,psi=psi,gamma=gamma,
    ##                                       parent.child.list=parent.child.list,
    ##                                       corpus.topic=corpus.topic)
    gradient.prior <- eval.mu.prior.grad(par,psi,hes.prior)
    log.mu.prior.grad <- gradient.prior[1:K]
    log.mu.0.prior.grad <- gradient.prior[K+1]
  }
  
  gradient.like <- eval.like.grad(mu.f=mu.f,tree.data.list=tree.data.list)
  gradient.mu <- gradient.like + log.mu.prior.grad
  gradient.tree <- c(gradient.mu,log.mu.0.prior.grad)
  
  # Get gradient for entire tree of parameters
  # Include chain rule for discrim params since guessing in log space
  if(max.tau2){gradient.tree <- c(gradient.tree,log.tau2.prior.grad*tau2f.vec)}
  names(gradient.tree) <- names(par)

  if(print.post.error){
    if(any(any(is.na(gradient.like)),any(is.nan(gradient.like)),
           any(is.na(log.mu.prior.grad)),any(is.nan(log.mu.prior.grad)))){
      print(list(gradient.like=gradient.like,gradient.tree=gradient.tree,
                 log.mu.prior.grad=log.mu.prior.grad))
    }}
  
  return(gradient.tree)
}


optim.tree <- function(job.id,current.param.list,doc.length.vec,
                       doc.topic.list,feature.count.list,topic.address.book,
                       corpus.topic="CORPUS",hessian=FALSE,tree.data.out=FALSE,
                       max.tau2=TRUE,dist.tau2="inv.chisq",get.prior.hes=FALSE,
                       Ndoc.case.control=NULL,print.post.error=TRUE){

  # Get old parameter values from last update
  mu.f.old <- current.param.list$mu.param.vecs[job.id,]
  mu.0.f.old <- current.param.list$mu.corpus.vec[job.id]
  tau2f.vec.old <- current.param.list$tau2.param.vecs[job.id,]
  
  # Start optimizer at old parameter values
  par <- c(mu.f.old,mu.0.f.old)
  if(max.tau2){par <- c(par,log(tau2f.vec.old))
  } else { get.prior.hes <- TRUE }
  
  tree.data.list <- get.data.for.tree(word.id=job.id,
                                      current.param.list=current.param.list,
                                      doc.length.vec=doc.length.vec,
                                      doc.topic.list=doc.topic.list,
                                      feature.count.list=feature.count.list,
                                      topic.address.book=topic.address.book,
                                      get.prior.hes=get.prior.hes,
                                      Ndoc.case.control=Ndoc.case.control)
  
  # Get starting value of posterior
  post.old <- tree.log.posterior(par=par,tree.data.list=tree.data.list,
                                 topic.address.book=topic.address.book,
                                 parent.child.list=
                                 current.param.list$parent.child.list,
                                 corpus.topic=corpus.topic,
                                 max.tau2=max.tau2,print.post.error=print.post.error)

  # Run optim
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
                          max.tau2=max.tau2,dist.tau2=dist.tau2,
                          hessian=hessian,print.post.error=print.post.error)

  # Check that optim converged
  optim.conv <- optim.out.bfgs$convergence == 0
  if(!optim.conv){
    optim.mes <- optim.out.bfgs$message
    warning(paste("Optim did not converge with message",optim.mes))  
  }
  
  optim.bfgs.par <- optim.out.bfgs$par
  K <- tree.data.list$K
  mu.f.new <- optim.bfgs.par[1:K]
  mu.0.f.new <- optim.bfgs.par[K+1]
  new.param.vec <- c(mu.f.new,mu.0.f.new)

  if(max.tau2){
    tau2f.vec.new <- exp(optim.bfgs.par[(K+2):length(optim.bfgs.par)])
    new.param.vec <- c(new.param.vec,tau2f.vec.new)
  }

  if(hessian){hessian.tree <- optim.out.bfgs$hessian}

  # Get new value of posterior
  post.new <- tree.log.posterior(par=optim.bfgs.par,
                                 tree.data.list=tree.data.list,
                                 topic.address.book=topic.address.book,
                                 parent.child.list=
                                 current.param.list$parent.child.list,
                                 corpus.topic=corpus.topic,
                                 max.tau2=max.tau2,print.post.error=print.post.error)

  #print(c(post.old,post.new))

  # Check global convergence of tree parameters
  global.conv <- check.conv(old.param.vec=post.old,
                            new.param.vec=post.new,
                            reltol=1e-6)
  
  out.list <- list(mu.f=mu.f.new,mu.0.f=mu.0.f.new,
                   global.conv=global.conv,
                   tree.post=post.new,tree.post.old=post.old)
  if(max.tau2){out.list$tau2f.vec <- tau2f.vec.new}
  if(hessian){out.list$hessian.tree <- hessian.tree}
  if(tree.data.out){out.list$tree.data.list <- tree.data.list}
  return(out.list)
}


# Function to perform Newton-Raphson optimization of tree parameters
nr.tree <- function(job.id,current.param.list,doc.length.vec,
                    doc.topic.list,feature.count.list,topic.address.book,
                    corpus.topic="CORPUS",hessian=FALSE,tree.data.out=FALSE,
                    max.iter=100,opt.step=FALSE,hes.diag.only=FALSE,
                    hes.n.sample=NULL){

  # Get old parameter values from last update
  mu.f.old <- current.param.list$mu.param.vecs[job.id,]
  mu.0.f.old <- current.param.list$mu.corpus.vec[job.id]
  tau2f.vec.old <- current.param.list$tau2.param.vecs[job.id,]
  K <- current.param.list$K
  
  # Start optimizer at old parameter values
  par <- c(mu.f.old,mu.0.f.old)
  par.start <- par
  
  tree.data.list <- get.data.for.tree(word.id=job.id,
                                      current.param.list=current.param.list,
                                      doc.length.vec=doc.length.vec,
                                      doc.topic.list=doc.topic.list,
                                      feature.count.list=feature.count.list,
                                      topic.address.book=topic.address.book,
                                      get.prior.hes=TRUE)

  for(i in 1:max.iter){

    if(i==1){
    # Get starting value of posterior
    post.old <- tree.log.posterior(par=par,tree.data.list=tree.data.list,
                                   topic.address.book=topic.address.book,
                                   parent.child.list=
                                   current.param.list$parent.child.list,
                                   corpus.topic=corpus.topic,
                                   max.tau2=FALSE)
    } else {post.old <- post.new}

    # Get gradient and Hessian at par
    grad <- tree.log.post.gradient(par=par,tree.data.list=tree.data.list,
                                   topic.address.book=topic.address.book,
                                   parent.child.list=
                                   current.param.list$parent.child.list,
                                   corpus.topic=corpus.topic,
                                   max.tau2=FALSE)
    hes <- eval.mu.hessian(par=par,tree.data.list=tree.data.list,
                           diag.only=hes.diag.only,n.sample=hes.n.sample)
    ## hes.inv <- -hessian2sigma(-hes)$hes.inv
    ## hes.inv <- solve(hes)
    ## change.par <- solve(hes,grad)
    ## par <- as.vector(par-change.par)
    ## names(par) <- names(par.start)
    
    # Get new parameter guess
    #par <- as.vector(par - step.size*hes.inv%*%grad)
    names(par) <- names(par.start)
    change.par <- solve(hes,grad)

    if(opt.step){
    # Get optimum step size
    step.opt <- optimize.step(par=par,change.par=change.par,
                              tree.data.list=tree.data.list,
                              topic.address.book=topic.address.book,
                              parent.child.list=
                              current.param.list$parent.child.list,
                              corpus.topic=corpus.topic)
    } else {step.opt <- 1}
    
    par <- as.vector(par - step.opt*change.par)
    names(par) <- names(par.start)

    # Get new value of posterior
    post.new <- tree.log.posterior(par=par,
                                   tree.data.list=tree.data.list,
                                   topic.address.book=topic.address.book,
                                   parent.child.list=
                                   current.param.list$parent.child.list,
                                   corpus.topic=corpus.topic,
                                   max.tau2=FALSE)

    # Check convergence of tree parameters
    conv <- check.conv(old.param.vec=post.old,
                       new.param.vec=post.new,
                       reltol=1e-6)
    if(conv){break}
  }

  # Save new values of parameters
  mu.f.new <- par[1:K]
  mu.0.f.new <- par[K+1]
  out.list <- list(mu.f=mu.f.new,mu.0.f=mu.0.f.new,
                   value=post.new)
  if(hessian){out.list$hessian.tree <- hes}
  if(tree.data.out){out.list$tree.data.list <- tree.data.list}
  return(out.list)
}

# Function to implement line search for best move
post.change <- function(step,par,change.par,
                        tree.data.list,topic.address.book,
                        parent.child.list,corpus.topic){

  out <- tree.log.posterior(par=par-step*change.par,
                            tree.data.list=tree.data.list,
                            topic.address.book=topic.address.book,
                            parent.child.list=parent.child.list,
                            corpus.topic=corpus.topic,
                            max.tau2=FALSE)
  return(out)
}

optimize.step <- function(par,change.par,tree.data.list,
                          topic.address.book,parent.child.list,
                          corpus.topic){

  step <- optimize(f=post.change,interval=c(0,1),par=par,
                   change.par=change.par,maximum=TRUE,
                   tree.data.list=tree.data.list,
                   topic.address.book=topic.address.book,
                   parent.child.list=parent.child.list,
                   corpus.topic=corpus.topic)$maximum
  return(step)
}


# Function to plot various projections of density
tree.post.proj <- function(index,par,npoints,range.points,current.param.list,
                           tree.data.list,
                           topic.address.book){
  half.range <- range.points/2
  par.get <- par[index]
  ruler <- seq(par.get-half.range,par.get+half.range,length.out=npoints)
  plot.mat <- matrix(par,nrow=npoints,ncol=length(par),byrow=TRUE)
  plot.mat[,index] <- ruler
  dens <- apply(plot.mat,1,tree.log.posterior,tree.data.list=tree.data.list,
                topic.address.book=topic.address.book,
                parent.child.list=
                current.param.list$parent.child.list,
                corpus.topic="CORPUS",max.tau2=FALSE)
  return(cbind(ruler,dens))
}
                
                               
                             

#########################
###### Graveyard ########
#########################

   ## X.active <- as.matrix(X.active)
    ## hessian.beta <- matrix(0,K,K)
    ## sapply(docs.use,function(i){
    ##   hessian.beta <<- hessian.beta + count.ratio[i]*X.active[i,]%*%t(X.active[i,])})

  ## # Get amplified X.prod.list
  ## X.cr.list <- mapply(function(X.prod,cr){cr*X.prod},X.prod=X.prod.list,
  ##                     cr=count.ratio,SIMPLIFY=FALSE)
  ## X.active.cr <- count.ratio*X[active.docs,,drop=FALSE]

  ## out <- apply(X.active.cr,1,function(x.vec){
  ##   hessian.beta <<- hessian.beta + x.vec%*%t(x.vec)})

  ## ave.hessian.beta <- matrix(0,K,K)
  ## n <- 1
  ## out <- apply(X.active.cr,1,function(x.vec){
  ##   ave.hessian.beta <<- ((n-1)/n)*ave.hessian.beta + (1/n)*x.vec%*%t(x.vec)
  ##   n <- n + 1
  ## })
  ## hessian.beta <- nrow(ave.hessian.beta)*ave.hessian.beta
  
  ## hessian.beta <- Reduce('+',X.cr.list)
  ## hessian.beta <- matrix(0,K,K)
  ## out <- lapply(X.cr.list,function(X.cr){
  ##   hessian.beta <<- hessian.beta + X.cr
  ## })
  
  ## hessian.beta <- matrix(0,K,K)
  ## out <- mapply(function(X.prod,cr){
  ##   hessian.beta <<- hessian.beta + cr*X.prod
  ## },X.prod=X.prod.list,cr=count.ratio)




## if(!is.null(Ndoc.case.control)){
##     N.samp <- Ndoc.case.control
##     N.inactive <- N.doc - N.active
##     weight.samp <- N.doc/N.samp

##     if(Ndoc.case.control > N.doc){
##       stop("User requested to sample more documents than in dataset")}

##     doc.names <- rownames(current.param.list$theta.param.vecs)

##     # If active docs more than half the total, just do random sample
##     if(N.active >= 0.5*N.doc){
##       doc.samp <- sample(doc.names,size=N.samp,replace=FALSE)
##       # Grab sampled docs
##       X <- X[doc.samp,]
##       doc.length.vec <- doc.length.vec[doc.samp]
##       active.docs <- active.docs[active.docs %in% doc.samp]
##       counts.feature <- counts.feature[active.docs]
##       X.active <- X.active[active.docs,,drop=FALSE]
##       # Can set case control option back to null since don't need to
##       # weight samples---can just proceed as normal
##       Ndoc.case.control <- NULL
    
##     # If active docs less than half the total but more than half of
##         # what we want to sample, sample from both active and
##         # inactive groups
##     } else if(all(N.active > 0.5*N.samp, N.active < 0.5*N.doc)){
##       N.active.samp <- trunc(N.samp/2)
##       weight.active.samp <- N.active/N.active.samp
##       N.inactive.samp <- N.samp - N.active.samp
##       weight.inactive.samp <- N.inactive/N.inactive.samp
##       inactive.docs <- doc.names[!(doc.names %in% active.docs)]
##       active.samp <- sample(active.docs,size=N.active.samp,replace=FALSE)
##       active.docs <- active.samp
##       inactive.samp <- sample(inactive.docs,size=N.inactive.samp,replace=FALSE)
##       doc.samp <- c(active.samp,inactive.samp)
##       # Grab sampled docs
##       X <- X[doc.samp,]
##       X.active <- X.active[active.docs,,drop=FALSE]
##       doc.length.vec <- doc.length.vec[doc.samp]
##       counts.feature <- counts.feature[active.samp]
      
##     # Else if the number of active documents less than half what you
##       # want to sample, keep them all and only sample inactive
##     } else {
##       N.active.samp <- N.active
##       N.inactive.samp <- N.samp - N.active.samp
##       weight.active.samp <- 1
##       weight.inactive.samp <- N.inactive/N.inactive.samp
##       inactive.docs <- doc.names[!(doc.names %in% active.docs)]
##       active.samp <- active.docs
##       inactive.samp <- sample(inactive.docs,size=N.inactive.samp,replace=FALSE)
##       doc.samp <- c(active.samp,inactive.samp)
##       # Grab sampled docs
##       X <- X[doc.samp,]
##       doc.length.vec <- doc.length.vec[doc.samp]
##     }}

##   if(all(!is.null(Ndoc.case.control),N.active < 0.5*N.doc)){
##     # Cache column sums of X for gradient calculation with weights
##     wX <- X
##     wX[active.samp,] <- weight.active.samp*wX[active.samp,]
##     wX[inactive.samp,] <- weight.inactive.samp*wX[inactive.samp,]
##     l.X.col.sums <- colSums(doc.length.vec*wX)   
##   } else {
##     # Cache column sums of X for gradient calculation without weights
##     l.X.col.sums <- colSums(doc.length.vec*X)
##   }
