# Functions for importance sampler of the tree parameters

library("mvtnorm")
library("geoR")

## # Proposal density
## mu.prop.dens <- function(mu.vec,prop.mean,prop.inv.cov){
##   deviat.vec <- mu.vec-prop.mean
##   log.prop.dens <- -0.5*deviat.vec%*%prop.inv.cov%*%deviat.vec
##   return(as.numeric(log.prop.dens))
## }

## # Function to generate proposal draws
## mu.prop.draw <- function(ndraws,prop.mean,prop.cov){
##   mu.vecs <- rmvnorm(n=ndraws,mean=prop.mean,sigma=prop.cov)
##   return(mu.vecs)
## }

# Function to evaluate importance weights of draws
# Takes N x (K+1) matrix of draws as input
tree.import.weights <- function(mu.draws,prop.mean,prop.Sigma.inv.chol,
                                topic.address.book,parent.child.list,
                                tree.data.list,current.param.list){
  # Get joint posterior density of mu.0.f and mu.f 
  target.log.dens <- apply(mu.draws,1,tree.log.posterior,
                           tree.data.list=tree.data.list,
                           topic.address.book=topic.address.book,
                           parent.child.list=
                           current.param.list$parent.child.list,
                           corpus.topic="CORPUS",max.tau2=FALSE)
  prop.log.dens <-  apply(mu.draws,1,norm.prop.dens,mean.vec=prop.mean,
                          Sigma.inv.chol=prop.Sigma.inv.chol)
  log.weights <- target.log.dens-prop.log.dens
  log.weights.center <- log.weights-mean(log.weights)
  #weights <- exp(log.weights.center)
  return(log.weights.center)
}

# Importance sampler
tree.import.sampler <- function(job.id,current.param.list,
                                doc.length.vec,doc.topic.list,
                                feature.count.list,topic.address.book,
                                ndraws=10000,prop.scale=1,par.start=NULL){
  
  # Get normal approx to posterior
  optim.out <- optim.tree(job.id=job.id,current.param.list=current.param.list,
                          doc.length.vec=doc.length.vec,
                          doc.topic.list=doc.topic.list,
                          feature.count.list=feature.count.list,
                          topic.address.book=topic.address.book,
                          corpus.topic="CORPUS",hessian=TRUE,
                          tree.data.out=TRUE,max.tau2=FALSE)
  K <- current.param.list$K
  prop.mean <- c(optim.out$mu.f,"mu.f.0"=optim.out$mu.0.f)
  hessian.out <- hessian2sigma(prop.scale*optim.out$hessian)
  prop.Sigma.chol <- hessian.out$Sigma.chol
  prop.Sigma.inv.chol <- hessian.out$Sigma.inv.chol
  
  #sigma.out <- hessian2sigma(-prop.scale*optim.out$hessian)
  #prop.inv.cov <- sigma.out$hes
  #prop.cov <- sigma.out$hes.inv
  ## prop.inv.cov <- -prop.scale*optim.out$hessian
  ## prop.cov <- solve(prop.inv.cov)
  ## prop.cov <- inv.hessian(prop.inv.cov)
  ## tau2f.vec <- optim.out$tau2f.vec
  tree.data.list <- optim.out$tree.data.list
  tau2f.vec <- tree.data.list$tau2f.vec
  
  # Get draws from proposal dist centered on posterior mode
  ## mu.draws <- mu.prop.draw(ndraws=ndraws,prop.mean=prop.mean,
  ##                          prop.cov=prop.cov)
  mu.draws <- norm.prop.draw(ndraws=ndraws,mean.vec=prop.mean,
                             Sigma.chol=prop.Sigma.chol)
  if(!is.null(par.start[1])){mu.draws <- rbind(par.start,mu.draws)
                           rownames(mu.draws) <- NULL}
  # Get important weights for samples
  log.weights <- tree.import.weights(mu.draws=mu.draws,
                                     prop.mean=prop.mean,
                                     #prop.inv.cov=prop.inv.cov,
                                     prop.Sigma.inv.chol=prop.Sigma.inv.chol,
                                     topic.address.book=topic.address.book,
                                     parent.child.list=parent.child.list,
                                     tree.data.list=tree.data.list,
                                     current.param.list=current.param.list)
  
  ## # Get expectation of sufficient stats
  ## expect.mean <- colMeans(mu.draws)
  ## expect.covar <- var(mu.draws)
  
  return(list(draws=mu.draws,log.weights=log.weights))
              ## expect.mean=expect.mean,
              ## expect.covar=expect.covar,))
}


# Function for conjugate conditional update for tau2.f
tau2.draw <- function(job.id,mu.f,mu.0.f,
                      current.param.list,
                      corpus.topic="CORPUS"){
  tau2f.vec.old <- current.param.list$tau2.param.vecs[job.id,]
  parent.child.list <- current.param.list$parent.child.list
  #mu.f <- current.param.list$mu.param.vecs[job.id,]
  #mu.0.f <- current.param.list$mu.corpus.vec[job.id]
  parent.topics <- names(tau2f.vec.old)
  nu <- current.param.list$nu
  sigma2 <- current.param.list$sigma2

  tau2f.vec.new <- sapply(parent.topics,get.tau2f.k.draw,mu.f=mu.f,
                          mu.0.f=mu.0.f,nu=nu,sigma2=sigma2,
                          parent.child.list=parent.child.list,
                          corpus.topic=corpus.topic)
  #names(tau2f.vec.new) <- parent.topics
  return(tau2f.vec.new)
}

# Function to get draw from tau2f.k conditional dist
get.tau2f.k.draw <- function(topic,mu.f,mu.0.f,nu,sigma2,
                             parent.child.list,corpus.topic){
  
  # Get parameters
  child.topics <- parent.child.list[[topic]]
  pos.children <- as.numeric(names(child.topics))
  mu.children <- mu.f[pos.children]
  if (topic==corpus.topic) {mu.self <- mu.0.f
  } else {mu.self <- mu.f[topic]}
  n.child <- length(mu.children)
  ss.child <- sum((mu.children-mu.self)^2)

  scale.numer <- nu*sigma2 + ss.child
  scale.denom <- nu + n.child
  scale <- scale.numer/scale.denom
  df <- n.child + nu

  # Draw new value for tau2.f.k
  tau2.f.k.new <- rinvchisq(n=1,df=df,scale=scale)
  
  return(tau2.f.k.new)
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


#################
# HMC functions #
#################


#source("/n/home13/jbischof/reuters_prj/hmc/hmc_functions.R")

U.tree <- function(...){-1*tree.log.posterior(...,max.tau2=FALSE)}
U.grad.tree <- function(...){-1*tree.log.post.gradient(...,max.tau2=FALSE)}

# Function to get HMC samples from tree posterior
hmc.tree <- function(job.id,ndraws,step.size,nsteps,
                     current.param.list,doc.length.vec,
                     doc.topic.list,feature.count.list,topic.address.book,
                     last.draw=FALSE,hessian.like=NULL,
                     corpus.topic="CORPUS",Ndoc.case.control=NULL,
                     n.sample.hes=1000,hes.diag=FALSE,debug=FALSE){

  # Get old parameter values from last update
  mu.f.old <- current.param.list$mu.param.vecs[job.id,]
  mu.0.f.old <- current.param.list$mu.corpus.vec[job.id]
  tau2f.vec.old <- current.param.list$tau2.param.vecs[job.id,]
  par <- c(mu.f.old,mu.0.f.old)

  # Get necessary items that only need to be computed once
  tree.data.list <- get.data.for.tree(word.id=job.id,
                                      current.param.list=current.param.list,
                                      doc.length.vec=doc.length.vec,
                                      doc.topic.list=doc.topic.list,
                                      feature.count.list=feature.count.list,
                                      topic.address.book=topic.address.book,
                                      get.prior.hes=TRUE,
                                      Ndoc.case.control=Ndoc.case.control)

  # Evaluate hessian at last draw position
  # Note that case control sampling does not affect hessian calculation
  # if the hessian.like is provided as an argument
  hes <- eval.mu.hessian(par,tree.data.list,diag.only=hes.diag,
                         n.sample=n.sample.hes,hessian.like=hessian.like)

  hes2mass.out <- hes2mass(hes=hes,hes.diag=hes.diag)
  M.inv <- hes2mass.out$M.inv
  M.sd <- hes2mass.out$M.sd
  M.diag <- hes2mass.out$M.diag
  
  # Run HMC
  out.hmc <- metro.hmc(ndraws=ndraws,q.start=par,nsteps=nsteps,
                       step.size=step.size,U=U.tree,U.grad=U.grad.tree,
                       M.sd=M.sd,M.inv=M.inv,M.diag=M.diag,
                       debug=debug,tree.data.list=tree.data.list,
                       topic.address.book=topic.address.book,
                       parent.child.list=
                       current.param.list$parent.child.list,
                       corpus.topic=corpus.topic)

  # Only return last draw if requested
  if(last.draw){out.hmc <- out.hmc[nrow(out.hmc),]}

  return(out.hmc)

}


# Code graveyard

## # Use only diagonal hessian if requested
##   if(hes.diag){
##     M <- -diag(hes)
##     M.sd <- sqrt(M)
##     M.inv <- diag(1/M)
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
##       M.inv <- diag(1/M)
##     } else {
##       M[M < 0] <- min(M[M > 0])
##       M.sd <- sqrt(M)
##       M.inv <- diag(1/M)
##       ## ave.diag <- mean(M)
##       ## M.sd <- rep(sqrt(ave.diag),length(M))
##       ## M.inv <- rep(1/ave.diag,length(M))
##     }
##     M.diag <- TRUE
##   }
