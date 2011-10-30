# Global coordinate ascent algorithm

library("Matrix")

hpd.gibbs.sampler <- function(current.param.list,
                              #feature.count.list=NULL,
                              #doc.count.list=NULL,
                              topic.address.book=NULL,
                              ndraws.gibbs=1500,Nupdate.hes=3,
                              verbose=FALSE,
                              print.iter=FALSE,debug=FALSE,
                              tree.job.list=NULL,xi.job.list=NULL,
                              file.current.param.list=NULL,
                              file.final.param.list=NULL,
                              file.ave.param.list=NULL,
                              tree.update=TRUE,xi.update=TRUE,
                              Ndoc.case.control=NULL,
                              Nfeat.case.control=NULL,
                              nwords.trace=50,ndocs.trace=50,
                              ndraws.save.final=5){

  # Set up schedule for updating hessian
  hes.sched <- sqrt.seq.sched(1,ndraws.gibbs,Nupdate.hes)

  # Create list of stored draws
  final.param.list <- setup.final.param.list(current.param.list=current.param.list,
                                             ndraws.gibbs=ndraws.gibbs,
                                             tree.update=tree.update,xi.update=xi.update,
                                             nwords.trace=nwords.trace,ndocs.trace=ndocs.trace)
  
  # Send out signal to slaves to load up their data
  n.slaves <- mpi.comm.size(0)
  for(slave.id in 1:n.slaves){
    mpi.send.Robj(obj=0,dest=slave.id,tag=88,comm=0)}

  # Write out initial current.param.list
  save(current.param.list,file=file.current.param.list)
  #cat(sprintf("\nNode %s\n",toString(mpi.comm.rank(0))))
  #print(gc())

  # Cycle through independent sets of draws
  for(i in 1:ndraws.gibbs){
    if(print.iter){cat(paste("\nGlobal iteration",i,"\n"))}

    if(tree.update){
      # Cycle through all words and draw tree parameters
      # Draw tree parameters using MPI
      # Tell slave nodes to prepare to draw tree parameters
      cat("Tree draws\n")
      master.param.fn(param.tag=5)
      # Refresh the likelihood hessian if on first iteration or each 100 iters
      update.hessian.like <- i %in% hes.sched
      tree.master.out <- tree.master.fn(tree.job.list=tree.job.list,
                                        current.param.list=current.param.list,
                                        update.hessian.like=update.hessian.like)
      current.param.list <- tree.master.out
      if(xi.update){
        # Write out new current.param.list to pass along to xi update
        save(current.param.list,file=file.current.param.list)
      }
    }

    
    if(xi.update){
      # Cycle through all docs and draw membership parameters
      # Draw xis using MPI
      # Tell slave nodes to prepare to draw xis
      cat("Xi draws\n")
      master.param.fn(param.tag=6)
      # Refresh the likelihood hessian if on first iteration or each 100 iters
      update.hessian.like <- i %in% hes.sched
      xi.master.out <- xi.master.fn(xi.job.list=xi.job.list,
                                    xi.param.vecs=
                                    current.param.list$xi.param.vecs,
                                    theta.param.vecs=
                                    current.param.list$theta.param.vecs,
                                    update.hessian.like=update.hessian.like)
      # Maintain sparse representation of theta matrix
      current.param.list$theta.param.vecs <-
        as(xi.master.out$theta.param.vecs,"sparseMatrix")
      current.param.list$xi.param.vecs <- xi.master.out$xi.param.vecs
    }


    # Draw hyperameters
    hparam.outlist <- hparam.draw(current.param.list=current.param.list,
                                  tree.update=tree.update,xi.update=xi.update)
    if(tree.update){
      current.param.list$psi <- hparam.outlist$psi
      current.param.list$gamma <- hparam.outlist$gamma
      current.param.list$nu <- hparam.outlist$nu
      current.param.list$sigma2 <- hparam.outlist$sigma2
    }
    if(xi.update){
      current.param.list$eta.vec <- hparam.outlist$eta.vec
      if(current.param.list$full.Sigma){
        current.param.list$Sigma <- hparam.outlist$Sigma
        hparam.outlist$Sigma <- diag(hparam.outlist$Sigma)
      } else {current.param.list$lambda2 <- hparam.outlist$lambda2}
    }
    print(hparam.outlist)

    
    # Write out new current.param.list
    save(current.param.list,file=file.current.param.list)
    

    # Store the latest draws
    final.param.list <- store.param.draws(i=i,current.param.list=current.param.list,
                                          final.param.list=final.param.list,
                                          #hparam.outlist=hparam.outlist,
                                          tree.update=tree.update,xi.update=xi.update)
    
    # Write out new final.param.list every ndraws.save.final draws
    if(i %% ndraws.save.final == 0){save(final.param.list,file=file.final.param.list)}
  }

  # Tell worker nodes to exit
  master.exit.fn()

  # Return final set of parameters
  return(final.param.list)
}

#########################################################################
#########################################################################
# Function to space updates
# First try in sqrt space
# Want most points in the beginning, none at end
sqrt.seq.sched <- function(from,to,length.out){
  sqrt.sched <- seq(sqrt(from),sqrt(to),length.out=length.out+1)
  sched.raw <- (sqrt.sched[-(length.out+1)])^2
  sched <- trunc(sched.raw)
  return(sched)
}

seq.sched <- function(from,to,length.out){
  sched.raw <- seq(from,to,length.out=length.out+1)
  sched <- trunc(sched.raw[-(length.out+1)])
  return(sched)
}

#########################################################################
#########################################################################

# Function to set up final.param.list
setup.final.param.list <- function(current.param.list,ndraws.gibbs,
                                   tree.update=TRUE,xi.update=TRUE,
                                   nwords.trace=NULL,ndocs.trace=NULL){
  K <- current.param.list$K
  D <- current.param.list$D
  V <- current.param.list$V
  nparents <- ncol(current.param.list$tau2.param.vecs)
  doc.names <- rownames(current.param.list$xi.param.vecs)
  parent.topic.names <- colnames(current.param.list$tau2.param.vecs)
  topic.names <- colnames(current.param.list$mu.param.vecs)
  word.names <- rownames(current.param.list$mu.param.vecs)
  
  final.param.list <- list()

  if(tree.update){
    if(!is.null(nwords.trace)){
      # Pick random sample of words to store
      words.trace <- sample(x=word.names,size=nwords.trace,replace=FALSE)
      word.names.trace <- word.names[words.trace]
    } else {
      nwords.trace <- V
      word.names.trace <- word.names
    }
    # Set up place to store tree parameters
    final.param.list$mu.corpus.vec <- matrix(NA,nrow=ndraws.gibbs,ncol=nwords.trace,
                                             dimnames=list(NULL,word.names.trace))
    final.param.list$mu.param.vecs <- array(NA,dim=c(nwords.trace,K,ndraws.gibbs),
                                            dimnames=list(word.names.trace,topic.names,NULL))
    final.param.list$tau2.param.vecs <- array(NA,dim=c(nwords.trace,nparents,ndraws.gibbs),
                                              dimnames=list(word.names.trace,parent.topic.names,NULL))
    final.param.list$psi <- rep(NA,ndraws.gibbs)
    final.param.list$gamma <- rep(NA,ndraws.gibbs)
    final.param.list$nu <- rep(NA,ndraws.gibbs)
    final.param.list$sigma2 <- rep(NA,ndraws.gibbs)
  }

  if(xi.update){
    # Set up place to store xi parameters
    # Pick random sample of docs to store
    if(!is.null(ndocs.trace)){
      docs.trace <- sample(x=doc.names,size=ndocs.trace,replace=FALSE)
      doc.names.trace <- doc.names[docs.trace]
    } else {
      ndocs.trace <- D
      doc.names.trace <- doc.names
    }
    final.param.list$xi.param.vecs <- array(NA,dim=c(ndocs.trace,K,ndraws.gibbs),
                                            dimnames=list(doc.names.trace,topic.names,NULL))
    final.param.list$eta.vec <- matrix(NA,nrow=ndraws.gibbs,ncol=K,
                                       dimnames=list(NULL,topic.names))
    if(current.param.list$full.Sigma){
      final.param.list$Sigma <- array(NA,dim=c(K,K,ndraws.gibbs),
                                            dimnames=list(topic.names,topic.names,NULL))
    } else {final.param.list$lambda2 <- rep(NA,ndraws.gibbs)}
    
  }
  
  return(final.param.list)
}


store.param.draws <- function(i,current.param.list,final.param.list,
                              tree.update=TRUE,xi.update=TRUE){

  if(tree.update){
    # Store tree parameters
    words.trace <- names(final.param.list$mu.corpus.vec)
    final.param.list$mu.corpus.vec[i,] <- current.param.list$mu.corpus.vec[words.trace]
    final.param.list$mu.param.vecs[,,i] <- current.param.list$mu.param.vecs[words.trace,]
    final.param.list$tau2.param.vecs[,,i] <- current.param.list$tau2.param.vecs[words.trace,]
    final.param.list$psi[i] <- current.param.list$psi
    final.param.list$gamma[i] <- current.param.list$gamma
    final.param.list$nu[i] <- current.param.list$nu
    final.param.list$sigma2[i] <- current.param.list$sigma2
  }

  if(xi.update){
    # Store xi parameters
    docs.trace <- rownames(final.param.list$xi.param.vecs[,,1])
    final.param.list$xi.param.vecs[,,i] <- current.param.list$xi.param.vecs[docs.trace,]
    final.param.list$lambda2[i] <- current.param.list$lambda2
    final.param.list$eta.vec[i,] <- current.param.list$eta.vec

    if(current.param.list$full.Sigma){
      final.param.list$Sigma[,,i] <- current.param.list$Sigma
    } else {final.param.list$lambda2[i] <- current.param.list$lambda2}
  }
  
  return(final.param.list)
}


# Function to update posterior expectation of parameters
update.average.params <- function(i,current.param.list,ave.param.list=list(),
                                  update="tree",hparam.outlist=NULL){

  # Get relative weights
  weight.current <- (i-1)/i
  weight.new <- 1/i
  
  if(update=="tree"){
    if(i==1){
      ave.param.list$mu.param.vecs <- current.param.list$mu.param.vecs
      ave.param.list$mu.corpus.vec <- current.param.list$mu.corpus.vec
      ave.param.list$tau2.param.vecs <- current.param.list$tau2.param.vecs
    } else {
      ave.param.list$mu.param.vecs <- weight.current*ave.param.list$mu.param.vecs +
        weight.new*current.param.list$mu.param.vecs
      ave.param.list$mu.corpus.vec <- weight.current*ave.param.list$mu.corpus.vec +
        weight.new*current.param.list$mu.corpus.vec
      ave.param.list$tau2.param.vecs <- weight.current*ave.param.list$tau2.param.vecs +
        weight.new*current.param.list$tau2.param.vecs
    }

    
  } else if(update=="xi"){
    if(i==1){
      ave.param.list$xi.param.vecs <- current.param.list$xi.param.vecs
    } else {
      ave.param.list$xi.param.vecs <- weight.current*ave.param.list$xi.param.vecs +
        weight.new*current.param.list$xi.param.vecs
    }
  
    ## } else if(update=="xi"){
    ##   #print(head(ave.param.list$xi.param.list))
    ##   if(i==1){
    ##     ave.param.list$xi.param.list <- current.param.list$xi.param.list
    ##   } else {
    ##     doc.ids <- names(current.param.list$xi.param.list)
    ##     ave.param.list$xi.param.list <- lapply(doc.ids,function(doc.id){
    ##       weight.current*ave.param.list$xi.param.list[[doc.id]] +
    ##       weight.new*current.param.list$xi.param.list[[doc.id]]})
    ##     names(ave.param.list$xi.param.list) <- doc.ids
    ##   }
        
        
    } else if(update=="hparam") {
        if(i==1){
          ave.param.list$psi <- hparam.outlist$psi
          ave.param.list$gamma <- hparam.outlist$gamma
          ave.param.list$nu <- hparam.outlist$nu
          ave.param.list$sigma2 <- hparam.outlist$sigma2
          ave.param.list$lambda2 <- hparam.outlist$lambda2
          ave.param.list$eta.vec <- hparam.outlist$eta.vec
        } else {
          ave.param.list$psi <- c(ave.param.list$psi,hparam.outlist$psi)
          ave.param.list$gamma <- c(ave.param.list$gamma,hparam.outlist$gamma)
          ave.param.list$nu <- c(ave.param.list$nu,hparam.outlist$nu)
          ave.param.list$sigma2 <- c(ave.param.list$sigma2,hparam.outlist$sigma2)
          ave.param.list$lambda2 <- c(ave.param.list$lambda2,hparam.outlist$lambda2)
        }
      }

      return(ave.param.list)
}


  ## # If using MPI and request "best" blocksize, calibrate here
  ## if(all(use.mpi,any(mpi.tree.block.size=="best",mpi.theta.block.size=="best"))){
  ##   n.slaves <- mpi.comm.size(comm=0)-1
  ##   if(mpi.tree.block.size=="best"){
  ##     n.words <- length(word.ids)
  ##     mpi.tree.block.size <- ceiling(n.words/n.slaves)
  ##   }
  ##   if(mpi.theta.block.size=="best"){
  ##     n.docs <- length(doc.ids)
  ##     mpi.theta.block.size <- ceiling(n.docs/n.slaves)
  ##   }
  ## }
