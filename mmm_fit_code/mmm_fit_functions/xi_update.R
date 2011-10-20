# Functions to update xi vector in mixed membership model

# Function to get params and data in useful format (anything that
# doesn't involve xi and should not be recalculated)
get.data.for.xi <- function(doc.id,mu.param.vecs,eta.vec,lambda2,
                            doc.length.vec,doc.count.list,
                            doc.topic.list=NULL,
                            classify=FALSE,active.only=TRUE,
                            Nfeat.case.control=NULL){

  # Make sure doc.id refers to id and not position
  doc.id <- toString(doc.id)
  
  # Get number of topics
  K <- ncol(mu.param.vecs)
  topics <- colnames(mu.param.vecs)
  
  # Get doc specific objects
  if(!classify){active.topics <- doc.topic.list[[doc.id]]}
  doc.norm.length <- doc.length.vec[doc.id]
  counts.doc <- doc.count.list[[doc.id]]
  active.words <- names(counts.doc)
  
  # Set up I.doc.vec
  if(!classify){
    if(active.only){
      I.doc.vec <- rep(1,length(active.topics))
      names(I.doc.vec) <- active.topics
    } else {
      I.doc.vec <- rep(0,K)
      names(I.doc.vec) <- topics
      I.doc.vec[active.topics] <- 1
  }} else {active.topics <- topics}

  # Figure out if only one topic active
  one.active <- length(active.topics) == 1

  # Only need to evaluate word likelihood if more than
  # one topic active
  if(!one.active){
    # mu.param.vecs is FxK
    # Only want mus from active topics
    if(classify){mu.param.mat <- mu.param.vecs
               } else {mu.param.mat <- mu.param.vecs[,active.topics]}
    X.d <- doc.norm.length*exp(mu.param.mat)
    # Get active rows of X matrix
    X.d.active <- X.d[active.words,,drop=FALSE]

    # Do case control sampling of covariate matrix if requested
    if(!is.null(Nfeat.case.control)){
      out.case.cont <- case.control.samp(X=X.d,X.active=X.d.active,
                                         N.samp=Nfeat.case.control,
                                         active.index=active.words)
      weight.active.samp <- out.case.cont$weight.active.samp
      weight.inactive.samp <- out.case.cont$weight.inactive.samp
      index.samp <- out.case.cont$index.samp
      active.samp <- out.case.cont$active.samp
      inactive.samp <- out.case.cont$inactive.samp
      X.d <- out.case.cont$X
      X.d.active <- out.case.cont$X.active
      X.d.col.sums <- out.case.cont$X.col.sums
      like.mult <- out.case.cont$like.mult
      case.control.correct <- out.case.cont$case.control.correct
      # Make sure that other data objects have only sampled units and
      # in correct order
      counts.doc <- counts.doc[active.samp]
      active.words <- active.samp
   
    } else {
      # Cache column sums of X for gradient calculation without weights
      X.d.col.sums <- colSums(X.d)
      case.control.correct <- FALSE
      like.mult <- NULL
    }

  # If only one active topic, no need for case control sampling
  } else {case.control.correct <- FALSE
          like.mult <- NULL}

  # Extract xi prior parameters
  eta.vec <- eta.vec[topics]
  if(active.only){
    Sigma <- lambda2*diag(length(active.topics))
    eta.vec <- eta.vec[active.topics]
    Sigma.inv <- (1/lambda2)*diag(length(active.topics))
  } else {
    Sigma <- lambda2*diag(length(topics))
    Sigma.inv <- (1/lambda2)*diag(length(topics))
  }
    
  xi.data.list <- list(counts.doc=counts.doc,I.doc.vec=I.doc.vec,
                       active.words=active.words,one.active=one.active,
                       doc.id=doc.id,eta.vec=eta.vec,Sigma=Sigma,
                       Sigma.inv=Sigma.inv,
                       case.control.correct=case.control.correct)

  if(!one.active){
    xi.data.list$X.d <- X.d
    xi.data.list$X.d.col.sums <- X.d.col.sums
  }

  # Save weight information if doing case control sampling and
  # need correction
  if(case.control.correct){
    xi.data.list$weight.active.samp <- out.case.cont$weight.active.samp
    xi.data.list$weight.inactive.samp <- out.case.cont$weight.inactive.samp
    xi.data.list$inactive.samp <- out.case.cont$inactive.samp
  }

  if(!classify){xi.data.list$active.topics <- active.topics}

  return(xi.data.list)
}


# Function to evaluate hessian of xi posterior
# Current takes hessian of like as parameter --- will have to
# get this numerically
eval.xi.hessian <- function(Sigma.inv,hessian.like){

  #print(head(hessian.like))
  #print(head(Sigma.inv))
  hes <- hessian.like - Sigma.inv
  
  return(hes)
}


xi.w.log.like <- function(xi.vec,xi.data.list,active.only=TRUE){

  # Get theta vector
  if(active.only){
    theta.d <- get.theta.from.xi(xi.vec)
    ## theta.d <- exp(xi.vec)/sum(exp(xi.vec))
  } else {  
    active.topics <- xi.data.list$active.topics
    xi.vec.active <- xi.vec[active.topics]
    theta.d <- get.theta.from.xi(xi.vec.active)
    ## active.exp.xi <- exp(xi.vec[active.topics])
    ## theta.d <- active.exp.xi/sum(active.exp.xi)
  }
  
  # Unpack needed data
  active.words <- xi.data.list$active.words
  X.d <- xi.data.list$X.d
  counts.doc <- xi.data.list$counts.doc
  
  # Get linear predictor
  # For now assuming xi.d a vector of active topics in right order
  x.theta.vec <- as.vector(X.d%*%theta.d)
  names(x.theta.vec) <- rownames(X.d)
  
  # Evaluate log likelihood
  # Use weighted sum if doing case control sampling
  if(xi.data.list$case.control.correct){
    weight.active.samp <- xi.data.list$weight.active.samp
    weight.inactive.samp <- xi.data.list$weight.inactive.samp
    inactive.samp <- xi.data.list$inactive.samp
    log.like <- -weight.inactive.samp*sum(x.theta.vec[inactive.samp]) -
      weight.active.samp*sum(x.theta.vec[active.words]) +
        weight.active.samp*sum(counts.doc*log(x.theta.vec[active.words]))

  } else {
    # Evaluate log likelihood of theta.vec
    log.like <- -sum(x.theta.vec) +
      sum(counts.doc*log(x.theta.vec[active.words]))
  }

  # If need to scale likelihood by constant, do this now
  if(!is.null(xi.data.list$like.mult)){
    log.like <- log.like*xi.data.list$like.mult}

  return(log.like)
}


# Function to evaluate gradient of log like of xi.vec
# Will evaluate in theta space and then use chain rule
xi.w.log.like.grad <- function(xi.vec,xi.data.list,active.only=TRUE){

  # Get theta vector
  if(active.only){
    theta.d <- get.theta.from.xi(xi.vec)
    ## theta.d <- exp(xi.vec)/sum(exp(xi.vec))
    xi.vec.active <- xi.vec
  } else {  
    active.topics <- xi.data.list$active.topics
    xi.vec.active <- xi.vec[active.topics]
    ## active.exp.xi <- exp(xi.vec.active)
    ## theta.d <- active.exp.xi/sum(active.exp.xi)
    theta.d <- get.theta.from.xi(xi.vec.active)
  }
  
  # Unpack needed data
  active.words <- xi.data.list$active.words
  X.d <- xi.data.list$X.d
  counts.doc <- xi.data.list$counts.doc
  X.d.col.sums <- xi.data.list$X.d.col.sums
  
  # Get active rows of X matrix
  X.d.active <- X.d[active.words,,drop=FALSE]

  # Get linear predictor
  # For now assuming xi.d a vector of active topics in right order
  x.theta.active <- as.vector(X.d.active%*%theta.d)

  # Get count ratio for active words
  count.ratio <- counts.doc/x.theta.active

  # Use weighted sum if doing case control sampling
  if(xi.data.list$case.control.correct){
    weight.active.samp <- xi.data.list$weight.active.samp
    weight.inactive.samp <- xi.data.list$weight.inactive.samp
    log.like.grad.theta <- -X.d.col.sums +
      weight.active.samp*as.vector(count.ratio%*%X.d.active)
  } else {
    # Evaluate gradient of log like in terms of theta
    log.like.grad.theta <- -X.d.col.sums + as.vector(count.ratio%*%X.d.active)
  }

  # If need to scale grad by constant, do this now
  if(!is.null(xi.data.list$like.mult)){
    log.like.grad.theta <- log.like.grad.theta*xi.data.list$like.mult}

  # Now need chain rule to get gradient terms of xi.vec
  chain.rule.mat <- xi.chain.rule(xi.vec.active)
  log.like.grad <- as.vector(chain.rule.mat%*%log.like.grad.theta)

  # If including inactive topics, need to pad out vector with zeros
  if(!active.only){
    log.like.grad.all <- rep(0,length(xi.vec))
    names(log.like.grad.all) <- names(xi.vec)
    log.like.grad.all[active.topics] <- log.like.grad
    log.like.grad <- log.like.grad.all
  }

  return(log.like.grad)
}


# Get matrix of chain rule to transform gradient to xi space
xi.chain.rule <- function(xi.vec){

  exp.xi.vec <- exp(xi.vec)
  denom <- sum(exp.xi.vec)^2

  numer.mat <- sapply(xi.vec,function(xi.k){-exp(xi.k+xi.vec)})
  diag(numer.mat) <- diag(numer.mat) + exp.xi.vec*sum(exp.xi.vec)
  chain.rule.mat <- numer.mat/denom

  return(chain.rule.mat)
}


# Function to evaluate log like of labels given xi
xi.label.log.like <- function(xi.vec,I.vec){
  log.like <- -sum(log(1+exp(-xi.vec)) + (1-I.vec)*xi.vec)
  return(log.like)
}


# Function to evaluate gradient of log like of labels given xi
xi.label.log.like.grad <- function(xi.vec,I.vec){
  grad <- (1/(1+exp(xi.vec)))-(1-I.vec)
  return(grad)
}


# Function to evaluate log prior of xi.vec
xi.log.prior <- function(xi.vec,eta.vec,Sigma.inv){

  deviat.vec <- xi.vec-eta.vec
  log.prior <- as.numeric(-0.5*t(deviat.vec)%*%Sigma.inv%*%deviat.vec)
  return(log.prior)
}


# Function to evaluate gradient of log prior of xi.vec
xi.log.prior.grad <- function(xi.vec,eta.vec,Sigma.inv){

  deviat.vec <- xi.vec-eta.vec
  log.prior.grad <- -as.vector(Sigma.inv%*%deviat.vec)
  return(log.prior.grad)
}


# Function to evaluate log conditional posterior of xi.d
xi.log.posterior <- function(par,xi.data.list,eta.vec,Sigma.inv,
                             one.active=FALSE,active.only=TRUE,
                             debug=FALSE,print.post.error=FALSE){

  # Get xi.vec
  xi.vec <- par
  
  # Get I.doc.vec
  I.doc.vec <- xi.data.list$I.doc.vec
  
  # Only have like contribution from words if more than one active topic
  if(!one.active){
    # Get log like of words
    w.log.like <- xi.w.log.like(xi.vec=xi.vec,xi.data.list=xi.data.list,
                                active.only=active.only)}
  
  # Get log like of labels
  label.log.like <- xi.label.log.like(xi.vec=xi.vec,I.vec=I.doc.vec)
  
  # Get log prior of xi
  log.prior <- xi.log.prior(xi.vec=xi.vec,eta.vec=eta.vec,Sigma.inv=Sigma.inv)
  
  # Get log posterior
  log.posterior <- ifelse(one.active,0,w.log.like) + label.log.like + log.prior

  if(print.post.error){
    if(any(is.na(log.posterior),log.posterior==Inf,log.posterior==-Inf,
           is.nan(log.posterior),is.null(log.posterior))){
      print.list <- list(xi.vec=xi.vec,
                         label.log.like=label.log.like,
                         log.prior=log.prior,par=par,
                         doc.id=xi.data.list$doc.id)
      if(!one.active){print.list$w.log.like <- w.log.like}
      print(print.list)}
  }
  
  return(as.numeric(log.posterior))
}


# Function to evaluate gradient of log conditional posterior of xi.d
xi.log.post.gradient <- function(par,xi.data.list,eta.vec,Sigma.inv,
                                 one.active=FALSE,active.only=TRUE,
                                 debug=FALSE,print.post.error=FALSE){

  # Get xi.vec
  xi.vec <- par

  # Get I.doc.vec
  I.doc.vec <- xi.data.list$I.doc.vec

  # Only have like contribution from words if more than one active topic
  if(!one.active){
    # Get gradient of log like of words
    w.log.like.grad <- xi.w.log.like.grad(xi.vec=xi.vec,xi.data.list=xi.data.list,
                                          active.only=active.only)}

  # Get gradient of log like of labels
  label.log.like.grad <- xi.label.log.like.grad(xi.vec=xi.vec,I.vec=I.doc.vec)

  # Get gradient of log prior
  log.prior.grad <- xi.log.prior.grad(xi.vec=xi.vec,eta.vec=eta.vec,Sigma.inv=Sigma.inv)

  # Get gradient of log posterior
  gradient <- label.log.like.grad + log.prior.grad
  if(!one.active){gradient <- gradient + w.log.like.grad}

  if(print.post.error){
    if(any(any(is.na(gradient)),any(gradient==Inf),any(gradient==-Inf),
           any(is.nan(gradient)),any(is.null(gradient)))){
      print.list <- list(xi.vec=xi.vec,
                         label.log.like.grad=label.log.like.grad,
                         log.prior.grad=log.prior.grad,par.grad=par,
                         doc.id=xi.data.list$doc.id,
                         active.topics=xi.data.list$active.topics)
      if(!one.active){print.list$w.log.like.grad <- w.log.like.grad}
      print(print.list)}
  }
  
  return(gradient)
}


optim.xi <- function(job.id,current.param.list,doc.length.vec,
                     doc.count.list,doc.topic.list=NULL,debug=FALSE,
                     classify=FALSE,hessian=FALSE,
                     xi.data.out=FALSE,active.only=TRUE,
                     Nfeat.case.control=NULL,print.post.error=TRUE){

  # Make sure using job.id as a string
  job.id <- as.character(job.id)
  
  # Get old xi.d from last update
  if(active.only){xi.d.old <- current.param.list$xi.param.list[[job.id]]
  } else {
    xi.d.old <- current.param.list$xi.param.vecs[job.id,]
    ## xi.d.old <- xi.d.old + rnorm(n=length(xi.d.old),sd=2)
  }

  # Construct data needed for optimization
  xi.data.list <- get.data.for.xi(doc.id=job.id,
                                  eta.vec=current.param.list$eta.vec,
                                  lambda2=current.param.list$lambda2,
                                  mu.param.vecs=current.param.list$mu.param.vecs,
                                  doc.length.vec=doc.length.vec,
                                  doc.count.list=doc.count.list,
                                  doc.topic.list=doc.topic.list,
                                  active.only=active.only,
                                  Nfeat.case.control=Nfeat.case.control)

  # Extract xi prior parameters
  active.topics <- xi.data.list$active.topics
  one.active <- xi.data.list$one.active
  eta.vec <- xi.data.list$eta.vec
  Sigma <- xi.data.list$Sigma
  Sigma.inv <- xi.data.list$Sigma.inv
  ## eta.vec <- current.param.list$eta.vec[names(xi.d.old)]
  ## lambda2 <- current.param.list$lambda2
  ## Sigma <- lambda2*diag(length(xi.d.old))
  ## Sigma.inv <- (1/lambda2)*diag(length(xi.d.old))
  
  # Evaluate posterior at old values
  post.old <- xi.log.posterior(par=xi.d.old,xi.data.list=xi.data.list,
                               eta.vec=eta.vec,Sigma.inv=Sigma.inv,
                               one.active=one.active,active.only=active.only,
                               print.post.error=print.post.error)
  
  # Optimize log posterior with BFGS
  optim.out <- optim(par=xi.d.old,fn=xi.log.posterior,
                     control=list(fnscale=-1),
                     xi.data.list=xi.data.list,
                     eta.vec=eta.vec,Sigma.inv=Sigma.inv,
                     one.active=one.active,active.only=active.only,
                     method="L-BFGS-B",upper=250,lower=-250,
                     gr=xi.log.post.gradient,
                     debug=debug,hessian=hessian,
                     print.post.error=print.post.error)

  # Check that optim converged
  optim.conv <- optim.out$convergence == 0
  if(!optim.conv){
    optim.mes <- optim.out$message
    warning(paste("Optim did not converge with message",optim.mes))  
  }
  
  xi.d.new <- optim.out$par
  if(hessian){hessian.xi <- optim.out$hessian}
  
  # Evaluate posterior at new values
  post.new <- xi.log.posterior(par=xi.d.new,xi.data.list=xi.data.list,
                               eta.vec=eta.vec,Sigma.inv=Sigma.inv,
                               one.active=one.active,active.only=active.only,
                               print.post.error=print.post.error)
  
  # Check global convergence of theta.d if not classifying
  global.conv <- check.conv(old.param.vec=post.old,
                            new.param.vec=post.new,
                            reltol=1e-6)
  
  out.list <- list(xi.d=xi.d.new,global.conv=global.conv)
  if(hessian){out.list$hessian <- hessian.xi}
  if(xi.data.out){
    ## xi.data.list$eta.vec <- eta.vec
    ## xi.data.list$Sigma.inv <- Sigma.inv
    ## xi.data.list$Sigma <- Sigma
    out.list$xi.data.list <- xi.data.list}
  
  return(out.list)
}


# Function to plot various projections of density
xi.post.proj <- function(index,par,npoints,range.points,
                         xi.data.list,type="post"){
  half.range <- range.points/2
  par.get <- par[index]
  ruler <- seq(par.get[1]-half.range,par.get[1]+half.range,length.out=npoints)
  if(length(index)==2){
    ruler2 <- seq(par.get[2]-half.range,par.get[2]+half.range,length.out=npoints)
    grid <- as.matrix(expand.grid(ruler,ruler2))
  } else {grid <- as.matrix(ruler)}
  plot.mat <- matrix(par,nrow=nrow(grid),ncol=length(par),byrow=TRUE)
  colnames(plot.mat) <- names(par)
  plot.mat[,index] <- grid
  if(type=="post"){
    dens <- apply(plot.mat,1,xi.log.posterior,
                  eta.vec=xi.data.list$eta.vec,Sigma.inv=xi.data.list$Sigma.inv,
                  xi.data.list=xi.data.list,active.only=FALSE)
  } else if(type=="wlike"){
    dens <- apply(plot.mat,1,xi.w.log.like,xi.data.list=xi.data.list,
                  active.only=FALSE)
  } else if(type=="ilike"){
    dens <- apply(plot.mat,1,xi.label.log.like,I.vec=xi.data.list$I.doc.vec)
    
  } else if(type=="prior"){
    dens <- apply(plot.mat,1,xi.log.prior,eta.vec=xi.data.list$eta.vec,
                  Sigma.inv=xi.data.list$Sigma.inv)
    
  } else{stop("Invalid plot type specified.")}

  if(length(index)==1){out.list <- list(x=ruler,dens=dens)
  } else {out.list <- list(x=ruler,y=ruler2,dens=dens)}
  
  return(out.list)
}


# Function to convert xis to thetas
get.theta.from.xi <- function(xi.vec){
  # Subtract off maximum from xis
  xi.center <- xi.vec - max(xi.vec)
  exp.xi <- exp(xi.center)
  theta.vec <- exp.xi/sum(exp.xi)
  return(theta.vec)
}
