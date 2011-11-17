# Functions to update xi vector in mixed membership model

library("mvtnorm")

# Proposal density
xi.prop.dens <- function(xi.vec,prop.mean,prop.inv.cov){
  deviat.vec <- xi.vec-prop.mean
  log.prop.dens <- -0.5*deviat.vec%*%prop.inv.cov%*%deviat.vec
  return(as.numeric(log.prop.dens))
}


# Function to generate proposal draws
xi.prop.draw <- function(ndraws,prop.mean,prop.cov){
  xi.vecs <- rmvnorm(n=ndraws,mean=prop.mean,sigma=prop.cov)
  return(xi.vecs)
}


# Function to evaluate importance weights of draws
# Takes N x (K+1) matrix of draws as input
xi.import.weights <- function(xi.draws,prop.mean,prop.inv.cov,
                              xi.data.list,current.param.list,
                              eta.vec,Sigma.inv){
  
  # Get joint density of each draw in target and proposal dists
  target.log.dens <- apply(xi.draws,1,xi.log.posterior,
                           xi.data.list=xi.data.list,
                           eta.vec=eta.vec,Sigma.inv=Sigma.inv)
  prop.log.dens <-  apply(xi.draws,1,xi.prop.dens,prop.mean=prop.mean,
                          prop.inv.cov=prop.inv.cov)
  log.weights <- target.log.dens-prop.log.dens
  log.weights.center <- log.weights-mean(log.weights)
  #weights <- exp(log.weights.center)
  return(log.weights.center)
}


# Importance sampler
xi.import.sampler <- function(job.id,current.param.list,
                              doc.length.vec,doc.topic.list,
                              doc.count.list,ndraws=10000,prop.scale=1,
                              par.start=NULL){
  
  # Get normal approx to posterior
  optim.out <- optim.xi(job.id=job.id,
                        current.param.list=current.param.list,
                        doc.length.vec=doc.length.vec,
                        doc.count.list=doc.count.list,
                        doc.topic.list=doc.topic.list,
                        hessian=TRUE,xi.data.out=TRUE)
 
  K <- current.param.list$K
  prop.mean <- optim.out$xi.d
  sigma.out <- hessian2sigma(-prop.scale*optim.out$hessian)
  prop.inv.cov <- sigma.out$hes
  prop.cov <- sigma.out$hes.inv
  xi.data.list <- optim.out$xi.data.list

  # Extract xi prior parameters
  active.topics <- xi.data.list$active.topics
  eta.vec <- current.param.list$eta.vec[active.topics]
  lambda2 <- current.param.list$lambda2
  Sigma <- lambda2*diag(length(active.topics))
  Sigma.inv <- (1/lambda2)*diag(length(active.topics))
  
  # Get draws from proposal dist centered on posterior mode
  xi.draws <- xi.prop.draw(ndraws=ndraws,prop.mean=prop.mean,
                           prop.cov=prop.cov)
  if(!is.null(par.start[1])){xi.draws <- rbind(par.start,xi.draws)
                             rownames(xi.draws) <- NULL}
  
  # Get important weights for samples
  log.weights <- xi.import.weights(xi.draws=xi.draws,prop.mean=prop.mean,
                                   prop.inv.cov=prop.inv.cov,
                                   xi.data.list=xi.data.list,
                                   current.param.list=current.param.list,
                                   eta.vec=eta.vec,Sigma.inv=Sigma.inv)
  
  
  return(list(draws=xi.draws,log.weights=log.weights))
}



#################
# HMC functions #
#################

U.xi <- function(...){-1*xi.log.posterior(...)}
U.grad.xi <- function(...){-1*xi.log.post.gradient(...)}

# Function to get HMC samples from xi posterior
hmc.xi <- function(job.id,ndraws,step.size,nsteps,
                   current.param.list,doc.count.list,
                   doc.length.vec,doc.topic.list,active.only=FALSE,
                   last.draw=FALSE,hessian.like=NULL,
                   hes.diag=FALSE,Nfeat.case.control=NULL,
                   debug=FALSE,classify=FALSE,pos.start=NULL){

  # Make sure using job.id as a string
  job.id <- as.character(job.id)

  # If given starting position, use that
  if(!is.null(pos.start)){
    xi.d.old <- pos.start
    #names(xi.d.old) <- colnames(current.param.list$xi.param.vecs)
  
  # Else get old xi.d from last update
  } else {
    if(active.only){xi.d.old <- current.param.list$xi.param.list[[job.id]]
    } else {
      xi.d.old <- current.param.list$xi.param.vecs[job.id,]
    }}

  # Construct data needed for optimization
  xi.data.list <- get.data.for.xi(doc.id=job.id,
                                  eta.vec=current.param.list$eta.vec,
                                  full.Sigma=current.param.list$full.Sigma,
                                  lambda2=current.param.list$lambda2,
                                  Sigma=current.param.list$Sigma,
                                  mu.param.vecs=current.param.list$mu.param.vecs,
                                  doc.length.vec=doc.length.vec,
                                  doc.count.list=doc.count.list,
                                  doc.topic.list=doc.topic.list,
                                  active.only=active.only,
                                  Nfeat.case.control=Nfeat.case.control,
                                  classify=classify)

  # Extract xi prior parameters
  active.topics <- xi.data.list$active.topics
  one.active <- xi.data.list$one.active
  eta.vec <- xi.data.list$eta.vec
  Sigma <- xi.data.list$Sigma
  Sigma.inv <- xi.data.list$Sigma.inv

  # Evaluate hessian at last draw position
  hes <- eval.xi.hessian(Sigma.inv=Sigma.inv,hessian.like=hessian.like)

  # Get mass matrix from hessian
  hes2mass.out <- hes2mass(hes=hes,hes.diag=hes.diag)
  M.inv <- hes2mass.out$M.inv
  M.sd <- hes2mass.out$M.sd
  M.diag <- hes2mass.out$M.diag
  
  # Run HMC
  out.hmc <- metro.hmc(ndraws=ndraws,q.start=xi.d.old,nsteps=nsteps,
                       step.size=step.size,U=U.xi,U.grad=U.grad.xi,
                       M.sd=M.sd,M.inv=M.inv,M.diag=M.diag,
                       debug=debug,xi.data.list=xi.data.list,
                       eta.vec=eta.vec,Sigma.inv=Sigma.inv,
                       one.active=one.active,active.only=active.only,
                       classify=classify)
  
  # Only return last draw if requested
  if(last.draw){out.hmc <- out.hmc[nrow(out.hmc),]}

  return(out.hmc)

}
