# Importance sampler for predictive distribution of xis and labels


# Function to get xi draws from proposal distribution
xi.prop.draw <- function(job.id,
                         ndraws.import.samp,
                         current.param.list,
                         doc.length.vec,
                         doc.count.list,
                         hmc.burnin=NULL,
                         hmc.step.size=0.1,
                         hmc.nsteps=20,
                         hmc.debug=FALSE){
  
  # Get mode of xi parameters for starting point and evaluating hessian
  optim.xi.out <- optim.xi(job.id=job.id,
                           current.param.list=current.param.list,
                           doc.length.vec=doc.length.vec,
                           doc.count.list=doc.count.list,
                           doc.topic.list=doc.topic.list,
                           xi.data.out=TRUE,hessian=TRUE,
                           active.only=FALSE,
                           classify=TRUE)
  
  xi.opt <- optim.xi.out$xi.d
  xi.data.list <- optim.xi.out$xi.data.list
  optim.hes <- optim.xi.out$hessian
  hessian.like <- optim.hes + xi.data.list$Sigma.inv
  
  # Get samples from proposal distribution using HMC
  xi.draws <- hmc.xi(job.id=job.id,ndraws=ndraws.import.samp,
                     step.size=hmc.step.size,nsteps=hmc.nsteps,
                     current.param.list=current.param.list,
                     doc.length.vec=doc.length.vec,
                     doc.count.list=doc.count.list,
                     hessian.like=hessian.like,
                     active.only=FALSE,
                     Nfeat.case.control=NULL,
                     classify=TRUE,pos.start=xi.opt,
                     debug=hmc.debug)

  # Remove burnin draws if requested
  if(!is.null(hmc.burnin)){xi.draws <- xi.draws[-c(1:hmc.burnin),]}

  out.list <- list(xi.draws=xi.draws,xi.data.list=xi.data.list,
                   xi.opt=xi.opt)

  return(out.list)
}


# Function to draw labels given xi proposals
# Need to load gen.I.vec function from process functions directory
label.prop.draw <- function(xi.draws){
  I.draws <- t(apply(xi.draws,1,gen.I.vec,gp="logit.norm"))
  return(I.draws)
}

# Function to evaluate density of label draw in target distribution
label.prop.dens <- function(xi.vec,xi.data.list){
  # Evaluate word log-likelihood
  w.like <- xi.w.log.like(xi.vec,xi.data.list,active.only=FALSE)
  return(w.like)
}

# Function to evaluate density of label draw in proposal distribution
label.target.dens <- function(xi.vec,I.vec,xi.data.list){
  # Modify xi.data.list with proposed labels
  active.topics <- xi.data.list$active.topics[I.vec == 1]
  xi.data.list$active.topics <- active.topics
  xi.data.list$X.d <- xi.data.list$X.d[,active.topics]

  # Evaluate word log-likelihood
  w.like <- xi.w.log.like(xi.vec,xi.data.list,active.only=FALSE)
  return(w.like)
}


# Function to evaluate importance weights of draws
label.import.weights <- function(xi.draws,I.draws,xi.data.list){

  ## # Figure out which I.vecs are duplicates
  ## I.string <- apply(I.vecs,1,paste,collapse="")
  ## dup.rows <- duplicated(I.string)
  ## I.unique <- I.vecs[-dup.rows,]
  ## xi.unique <- xi.vecs[-dup.rows,]
  ## n.draws <- nrow(xi.unique)

  n.draws <- nrow(xi.draws)
  
  # Get joint density of each draw in target and proposal dists
  target.log.dens <- sapply(1:n.draws,
        function(i,xi.draws,I.draws,xi.data.list){
          out <- label.target.dens(xi.vec=xi.draws[i,],
          I.vec=I.draws[i,],
          xi.data.list=xi.data.list)
          return(out)},
       xi.draws=xi.draws,I.draws=I.draws,
       xi.data.list=xi.data.list) 
  prop.log.dens <-  apply(xi.draws,1,label.prop.dens,
                          xi.data.list=xi.data.list)
  log.weights <- target.log.dens-prop.log.dens
  log.weights.center <- log.weights-mean(log.weights)
  return(log.weights.center)
}


# Label importance sampler
label.import.sampler <- function(job.id,current.param.list,
                                 doc.length.vec,doc.count.list,
                                 ndraws.import.samp=200,
                                 return.expect=FALSE,
                                 hmc.burnin=NULL,
                                 hmc.step.size=0.1,
                                 hmc.nsteps=20,
                                 hmc.debug=FALSE){

  # Get xi draws from proposal distribution
  xi.prop.out <- xi.prop.draw(job.id=job.id,
                              ndraws.import.samp=ndraws.import.samp,
                              current.param.list=current.param.list,
                              doc.length.vec=doc.length.vec,
                              doc.count.list=doc.count.list,
                              hmc.burnin=hmc.burnin,
                              hmc.step.size=hmc.step.size,
                              hmc.nsteps=hmc.nsteps,
                              hmc.debug=hmc.debug)

  xi.draws <- xi.prop.out$xi.draws
  xi.data.list <- xi.prop.out$xi.data.list
  xi.opt <- xi.prop.out$xi.opt

  # Get I draws given xi draws
  I.draws <- label.prop.draw(xi.draws)

  # Evaluate importance weights for each draw
  log.weights <- label.import.weights(xi.draws=xi.draws,
                                      I.draws=I.draws,
                                      xi.data.list=xi.data.list)

  # Return either expectation of label vector or posterior mode
  if(return.expect){
    weights <- exp(log.weights)
    norm.weights <- weights/sum(weights)
    out.list <- colMeans(norm.weights*I.draws)
  } else {
    out.list <- list(draws=I.draws,log.weights=log.weights) }
  
  return(out.list)
}
